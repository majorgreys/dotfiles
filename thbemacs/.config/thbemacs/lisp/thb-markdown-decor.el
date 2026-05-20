;;; thb-markdown-decor.el --- In-place decoration for markdown-ts-mode  -*- lexical-binding: t; -*-
;;
;; Author: Tahir Butt
;; Keywords: text, markdown, decoration
;;
;;; Commentary:
;;
;; `org-modern' / `org-appear' style in-place rendering for buffers in
;; `markdown-ts-mode'.  Walks the markdown-inline tree-sitter parse to
;; mark up "syntactic noise" (emphasis delimiters, code-span backticks,
;; link URLs) as invisible, so the source buffer reads as the rendered
;; content while you're editing.
;;
;; When point enters a decorated node (e.g. you click on a bolded word
;; or a link), the surrounding delimiters reveal automatically so you
;; can edit them; they re-hide when point leaves.
;;
;; Enable with:
;;
;;   M-x thb-markdown-decor-mode
;;
;; Or auto-enable in markdown-ts-mode:
;;
;;   (add-hook 'markdown-ts-mode-hook #'thb-markdown-decor-mode)
;;
;; Coverage in v0.1:
;;   - emphasis        *foo* / _foo_  → foo (italic face from existing
;;                                          treesit-font-lock-rules)
;;   - strong_emphasis **foo**        → foo (bold)
;;   - strikethrough   ~~foo~~        → foo (strikethrough)
;;   - code_span       `foo`          → foo (code face)
;;   - inline_link     [text](url)    → text (link face); URL revealed on
;;                                       cursor-entry, also as help-echo
;;
;; Not yet:
;;   - Inline images   ![alt](src)    → actual image (overlay display)
;;   - Reference-style links / link reference definitions
;;   - Code-fence language injection in source (live syntax highlighting)
;;   - Block quotes with line-prefix
;;   - HTML blocks
;;
;;; Code:

(require 'treesit)
(require 'cl-lib)
(require 'subr-x)

;;;; Customization -------------------------------------------------------

(defgroup thb-markdown-decor nil
  "In-place decoration for `markdown-ts-mode'."
  :group 'text
  :prefix "thb-markdown-decor-")

(defcustom thb-markdown-decor-decorated-types
  '("emphasis" "strong_emphasis" "strikethrough" "code_span" "inline_link")
  "Tree-sitter node types `thb-markdown-decor-mode' will decorate.
Use `setq-local' in a hook if you want to disable a specific kind in
some buffers without disabling the whole mode."
  :type '(repeat string)
  :group 'thb-markdown-decor)

(defcustom thb-markdown-decor-show-link-url-echo t
  "If non-nil, link URLs surface in the echo area on cursor-entry.
Also attached as `help-echo' on the link text so a mouse hover shows it."
  :type 'boolean
  :group 'thb-markdown-decor)

;;;; Internal state -----------------------------------------------------

(defconst thb-markdown-decor--invisibility-symbol 'thb-markdown-decor
  "Symbol added to `buffer-invisibility-spec' to hide decorated ranges.")

(defvar-local thb-markdown-decor--revealed nil
  "Cons (START . END) of the currently-revealed decorated region, or nil.
Set when point enters a decorated node; cleared when point leaves and
the region is re-hidden.")

;; Forward declaration so byte-compiler doesn't warn about the mode
;; variable being free at definition time.
(defvar thb-markdown-decor-mode)

;;;; Parser access ------------------------------------------------------

(defun thb-markdown-decor--inline-parser ()
  "Return the markdown-inline parser attached to the current buffer, or nil."
  (car (treesit-parser-list nil 'markdown-inline)))

(defun thb-markdown-decor--inline-children-in-range (start end)
  "Direct children of the markdown-inline parser's root in [START, END]."
  (when-let ((parser (thb-markdown-decor--inline-parser)))
    (seq-filter
     (lambda (n)
       (let ((ns (treesit-node-start n)))
         (and (>= ns start) (< ns end))))
     (treesit-node-children (treesit-parser-root-node parser)))))

;;;; Decoration walk -----------------------------------------------------

(defun thb-markdown-decor--child-of-type (parent type)
  "First child of PARENT whose `treesit-node-type' equals TYPE."
  (seq-find (lambda (c) (equal (treesit-node-type c) type))
            (treesit-node-children parent)))

(defun thb-markdown-decor--double-tilde-strikethrough-p (node)
  "Return non-nil when NODE is an exact ~~strikethrough~~ node.

The markdown-inline grammar also treats single-tilde `~foo~' spans as
`strikethrough'.  Do not decorate those: single tildes are common in notes
and paths, and false positives make large regions look deleted.  Triple-or-
longer tilde runs are also left alone instead of guessed at."
  (let ((start (treesit-node-start node))
        (end   (treesit-node-end node)))
    (and (equal (treesit-node-type node) "strikethrough")
         ;; Minimum non-empty exact form: ~~a~~.
         (>= (- end start) 5)
         (not (eq (char-before start) ?~))
         (eq (char-after start) ?~)
         (eq (char-after (1+ start)) ?~)
         (not (eq (char-after (+ start 2)) ?~))
         (eq (char-before end) ?~)
         (eq (char-before (1- end)) ?~)
         (not (eq (char-before (- end 2)) ?~))
         (not (eq (char-after end) ?~)))))

(defun thb-markdown-decor--decoratable-node-p (node)
  "Return non-nil when NODE is one this mode should decorate."
  (let ((type (treesit-node-type node)))
    (and (member type thb-markdown-decor-decorated-types)
         (or (not (equal type "strikethrough"))
             (thb-markdown-decor--double-tilde-strikethrough-p node)))))

(defun thb-markdown-decor--hide-emphasis-delimiters (node)
  "Hide all `emphasis_delimiter' descendants under NODE."
  (dolist (c (treesit-node-children node))
    (if (equal (treesit-node-type c) "emphasis_delimiter")
        (thb-markdown-decor--hide-range
         (treesit-node-start c) (treesit-node-end c))
      (thb-markdown-decor--hide-emphasis-delimiters c))))

(defun thb-markdown-decor--hide-range (start end)
  "Mark [START, END) invisible under our spec symbol.
Idempotent; calling twice is fine."
  (when (< start end)
    (put-text-property start end 'invisible
                       thb-markdown-decor--invisibility-symbol)))

(defun thb-markdown-decor--decorate-node (node)
  "Apply decoration text-properties to NODE."
  (let ((type (treesit-node-type node)))
    (cond
     ;; Emphasis-family containers: hide delimiter children.  For
     ;; strikethrough, only accept the GFM double-tilde form; the grammar also
     ;; parses `~foo~', which is too eager for source-buffer decoration.
     ((member type '("emphasis" "strong_emphasis"))
      (thb-markdown-decor--hide-emphasis-delimiters node))
     ((equal type "strikethrough")
      (when (thb-markdown-decor--double-tilde-strikethrough-p node)
        (thb-markdown-decor--hide-emphasis-delimiters node)))
     ;; code_span: hide its code_span_delimiter children.
     ((equal type "code_span")
      (dolist (c (treesit-node-children node))
        (when (equal (treesit-node-type c) "code_span_delimiter")
          (thb-markdown-decor--hide-range
           (treesit-node-start c) (treesit-node-end c)))))
     ;; inline_link [text](url):
     ;;   - hide opening `['
     ;;   - hide closing `](url)'
     ;;   - attach the URL as help-echo on the visible text
     ((equal type "inline_link")
      (let* ((link-start (treesit-node-start node))
             (link-end   (treesit-node-end   node))
             (text       (thb-markdown-decor--child-of-type node "link_text"))
             (dest       (thb-markdown-decor--child-of-type node "link_destination")))
        (when text
          (let ((ts (treesit-node-start text))
                (te (treesit-node-end   text)))
            ;; link_text in this grammar version is the INNER text only
            ;; (no enclosing brackets).  Hide just the bracket at
            ;; (link-start..ts), and the tail `](url)' at (te..link-end).
            (thb-markdown-decor--hide-range link-start ts)
            (thb-markdown-decor--hide-range te link-end)
            ;; Help-echo on the visible text region.
            (when (and dest thb-markdown-decor-show-link-url-echo)
              (let ((url-text (buffer-substring-no-properties
                               (treesit-node-start dest)
                               (treesit-node-end   dest))))
                (put-text-property ts te 'help-echo url-text)
                (put-text-property ts te 'thb-markdown-decor-url
                                   url-text))))))))))

(defun thb-markdown-decor--decorate-range (start end)
  "Walk inline parse children in [START, END] and decorate them.
Children outside the range are skipped."
  (when-let ((parser (thb-markdown-decor--inline-parser)))
    (let ((root (treesit-parser-root-node parser)))
      (dolist (child (treesit-node-children root))
        (let ((cs (treesit-node-start child)))
          (when (and (>= cs start) (< cs end))
            (when (thb-markdown-decor--decoratable-node-p child)
              (thb-markdown-decor--decorate-node child))))))))

(defun thb-markdown-decor--apply ()
  "Re-decorate the entire buffer (called on enable, full refresh)."
  (with-silent-modifications
    (thb-markdown-decor--decorate-range (point-min) (point-max))))

(defun thb-markdown-decor--clear-range (start end)
  "Remove our invisibility + help-echo properties in [START, END]."
  (with-silent-modifications
    (let ((pos start))
      (while (< pos end)
        (let ((next (next-single-property-change pos 'invisible nil end)))
          ;; Only clear `invisible' where the value is our symbol; don't
          ;; trample on other minor modes that may set it.
          (when (eq (get-text-property pos 'invisible)
                    thb-markdown-decor--invisibility-symbol)
            (put-text-property pos next 'invisible nil))
          (setq pos next)))
      ;; Same for help-echo we own.
      (let ((p start))
        (while (< p end)
          (let ((next (next-single-property-change p 'thb-markdown-decor-url nil end)))
            (when (get-text-property p 'thb-markdown-decor-url)
              (remove-text-properties p next '(help-echo nil thb-markdown-decor-url nil)))
            (setq p next)))))))

(defun thb-markdown-decor--clear ()
  "Remove all our decoration text-properties from the buffer."
  (thb-markdown-decor--clear-range (point-min) (point-max)))

;;;; Cursor reveal ------------------------------------------------------

(defun thb-markdown-decor--decorated-parent-at (pos)
  "Smallest ancestor at POS whose node-type is in
`thb-markdown-decor-decorated-types', AND that actually contains POS.

Guard against `treesit-node-at' returning the next node when POS sits
in plain text -- in that case `treesit-parent-until' happily walks up
from a nearby node and we'd falsely reveal something the cursor isn't
actually inside.  Verify range containment before returning."
  (when-let* ((parser (thb-markdown-decor--inline-parser))
              (node (treesit-node-at pos 'markdown-inline))
              (parent (treesit-parent-until
                       node
                       #'thb-markdown-decor--decoratable-node-p
                       t)))
    (when (and (<= (treesit-node-start parent) pos)
               (< pos (treesit-node-end parent)))
      parent)))

(defun thb-markdown-decor--reveal (node)
  "Reveal the decorations within NODE.
Removes invisibility within NODE's range and records the range in
`thb-markdown-decor--revealed' so we can re-hide on cursor exit."
  (let ((start (treesit-node-start node))
        (end   (treesit-node-end   node)))
    (thb-markdown-decor--clear-range start end)
    (setq thb-markdown-decor--revealed (cons start end))
    ;; Echo URL for links.
    (when (and (equal (treesit-node-type node) "inline_link")
               thb-markdown-decor-show-link-url-echo)
      (when-let ((dest (thb-markdown-decor--child-of-type node "link_destination")))
        (let ((url (buffer-substring-no-properties
                    (treesit-node-start dest)
                    (treesit-node-end   dest))))
          (let ((message-log-max nil)) (message "%s" url)))))))

(defun thb-markdown-decor--rehide ()
  "Re-apply decorations over the currently-revealed range."
  (when-let ((r thb-markdown-decor--revealed))
    (let ((start (car r)) (end (cdr r)))
      (with-silent-modifications
        (thb-markdown-decor--decorate-range start end))
      (setq thb-markdown-decor--revealed nil))))

(defun thb-markdown-decor--post-command ()
  "Manage reveal/rehide based on current point position."
  (when (and thb-markdown-decor-mode
             (thb-markdown-decor--inline-parser))
    (let* ((p (point))
           (was thb-markdown-decor--revealed))
      (cond
       ;; Still inside the previously-revealed range: do nothing.
       ((and was (>= p (car was)) (<= p (cdr was)))
        nil)
       (t
        (when was (thb-markdown-decor--rehide))
        (when-let ((parent (thb-markdown-decor--decorated-parent-at p)))
          (thb-markdown-decor--reveal parent)))))))

;;;; Change tracking ----------------------------------------------------

(defvar-local thb-markdown-decor--pending-changes nil
  "List of (START . END) ranges that need re-decoration.
Coalesced from `after-change-functions' calls and processed via an
idle timer to avoid re-walking on every keystroke.")

(defvar thb-markdown-decor--idle-delay 0.05
  "Idle delay before processing pending changes.")

(defun thb-markdown-decor--after-change (start end _len)
  "Record a pending re-decoration range."
  (push (cons start end) thb-markdown-decor--pending-changes)
  (run-with-idle-timer thb-markdown-decor--idle-delay nil
                       #'thb-markdown-decor--process-pending
                       (current-buffer)))

(defun thb-markdown-decor--process-pending (buf)
  "Re-decorate any pending change ranges in BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when thb-markdown-decor--pending-changes
        (let ((ranges thb-markdown-decor--pending-changes))
          (setq thb-markdown-decor--pending-changes nil)
          ;; Coalesce: find min start and max end across all ranges,
          ;; expand a bit, and re-decorate that whole span.  For typical
          ;; small edits this is cheap.
          (let ((min-start (apply #'min (mapcar #'car ranges)))
                (max-end   (apply #'max (mapcar #'cdr ranges))))
            ;; Snap to nearest line boundaries; markdown-inline nodes
            ;; don't cross paragraph boundaries so this is safe.
            (setq min-start (save-excursion (goto-char min-start) (line-beginning-position)))
            (setq max-end   (save-excursion (goto-char max-end)   (line-end-position)))
            (with-silent-modifications
              (thb-markdown-decor--clear-range min-start max-end)
              (thb-markdown-decor--decorate-range min-start max-end))))))))

;;;; Minor mode ---------------------------------------------------------

;;;###autoload
(define-minor-mode thb-markdown-decor-mode
  "Decorate markdown source in place: hide delimiters, render link text.

Requires the buffer to be in `markdown-ts-mode' (or any mode with the
markdown-inline tree-sitter parser attached).  When enabled, walks the
inline parse and applies `invisible' text-properties to:

  - emphasis / strong / strikethrough delimiter pairs
  - inline-code backticks
  - the closing tail of inline links (URL shown via help-echo)

Point-entry into a decorated node reveals its delimiters automatically;
they re-hide on cursor exit.

Use \[thb-markdown-decor-mode] to toggle, or auto-enable via:

  (add-hook \='markdown-ts-mode-hook #\='thb-markdown-decor-mode)"
  :lighter " mdr"
  :group 'thb-markdown-decor
  (cond
   (thb-markdown-decor-mode
    (unless (thb-markdown-decor--inline-parser)
      (user-error "No markdown-inline tree-sitter parser in this buffer"))
    (add-to-invisibility-spec thb-markdown-decor--invisibility-symbol)
    (add-hook 'post-command-hook #'thb-markdown-decor--post-command nil t)
    (add-hook 'after-change-functions #'thb-markdown-decor--after-change nil t)
    (thb-markdown-decor--apply))
   (t
    (remove-from-invisibility-spec thb-markdown-decor--invisibility-symbol)
    (remove-hook 'post-command-hook #'thb-markdown-decor--post-command t)
    (remove-hook 'after-change-functions #'thb-markdown-decor--after-change t)
    (when thb-markdown-decor--revealed
      (setq thb-markdown-decor--revealed nil))
    (thb-markdown-decor--clear))))

(provide 'thb-markdown-decor)
;;; thb-markdown-decor.el ends here
