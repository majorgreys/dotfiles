;;; thb-markdown-decor.el --- In-place decoration for markdown-ts-mode  -*- lexical-binding: t; -*-
;;
;; Author: Tahir Butt
;; Keywords: text, markdown, decoration
;;
;;; Commentary:
;;
;; `org-modern' / `org-appear' style in-place rendering for buffers in
;; `markdown-ts-mode'.  Queries the markdown-inline tree-sitter parse to
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
;; Coverage:
;;   - emphasis        *foo* / _foo_  → foo (italic face from existing
;;                                          treesit-font-lock-rules)
;;   - strong_emphasis **foo**        → foo (bold)
;;   - strikethrough   ~~foo~~        → foo (strikethrough)
;;   - code_span       `foo`          → foo (code face)
;;   - inline_link     [text](url)    → text (link face); URL revealed on
;;                                       cursor-entry, also as help-echo
;;
;;   Nested constructs are decorated too: the backticks in **a `b` c**, the
;;   URL in *see [x](y)*, and the stars in [**q**](url) all hide.
;;
;; Not yet:
;;   - Inline images   ![alt](src)    → actual image (overlay display)
;;   - Reference-style links / link reference definitions
;;   - Code-fence language injection in source (live syntax highlighting)
;;   - Block quotes with line-prefix
;;   - HTML blocks
;;
;; Implementation notes:
;;
;; Decoration is driven by `jit-lock', so only text that is actually
;; displayed is walked, and jit-lock owns change tracking, narrowing and
;; re-decoration after edits.  Node lookup is scoped to the markdown-inline
;; local parsers that overlap the requested region rather than walking every
;; parser in the buffer, and node matching uses a compiled tree-sitter query
;; instead of a hand-rolled child walk.  On a 2000-paragraph buffer that is
;; the difference between ~5 ms and ~0.01 ms for a single-line re-decoration.
;;
;;; Code:

(require 'treesit)
(require 'jit-lock)
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
Also attached as `help-echo' on the link text so a mouse hover shows it,
but only where nothing else already owns `help-echo' there."
  :type 'boolean
  :group 'thb-markdown-decor)

;;;; Internal state -----------------------------------------------------

(defconst thb-markdown-decor--invisibility-symbol 'thb-markdown-decor
  "Symbol added to `buffer-invisibility-spec' to hide decorated ranges.")

(defvar-local thb-markdown-decor--revealed nil
  "Cons (START-MARKER . END-MARKER) of the revealed decorated node, or nil.
Markers rather than integers so the range tracks edits made while the
node is revealed -- revealing a node in order to edit it is the whole
point, so the range must survive that edit.  Set when point enters a
decorated node; cleared when point leaves and the region is re-hidden.")

(defvar-local thb-markdown-decor--query-cache nil
  "Cons (TYPES . COMPILED-QUERY) memoising the node-capture query.")

;; Forward declaration so byte-compiler doesn't warn about the mode
;; variable being free at definition time.
(defvar thb-markdown-decor-mode)

;;;; Parser + query access ----------------------------------------------

(defun thb-markdown-decor--inline-parsers-in-range (start end)
  "Markdown-inline parsers whose text overlaps [START, END].

Emacs 31's built-in `markdown-ts-mode' creates one tagged *local*
markdown-inline parser per inline range, so a large document has as many
parsers as it has inline chunks.  Local parsers hang off overlays (this is
how `treesit-parsers-at' finds them), so `overlays-in' gives us just the
handful that matter for this region instead of the whole buffer's worth.

Only when no local parser covers the region do we fall back to plain global
markdown-inline parsers, which mirrors the local-takes-priority rule that
`treesit-node-at' and `treesit-parsers-at' already use.  The query is
range-restricted either way, so the fallback stays correct."
  (let (res)
    (dolist (ov (overlays-in (max (point-min) (1- start))
                             (min (point-max) (1+ end))))
      (let ((parser (overlay-get ov 'treesit-parser)))
        (when (and (treesit-parser-p parser)
                   (eq (treesit-parser-language parser) 'markdown-inline)
                   (not (memq parser res)))
          (push parser res))))
    (or res (treesit-parser-list nil 'markdown-inline))))

(defun thb-markdown-decor--query ()
  "Compiled markdown-inline query capturing `thb-markdown-decor-decorated-types'.
Recompiled only when the type list changes."
  (let ((types thb-markdown-decor-decorated-types))
    (if (equal (car thb-markdown-decor--query-cache) types)
        (cdr thb-markdown-decor--query-cache)
      (let ((query
             (condition-case err
                 (treesit-query-compile
                  'markdown-inline
                  (list (apply #'vector
                               (mapcar (lambda (ty) (list (intern ty))) types))
                        '@node)
                  t)
               (treesit-query-error
                (user-error
                 "Invalid `thb-markdown-decor-decorated-types' %S: %S"
                 types (cdr err))))))
        (setq thb-markdown-decor--query-cache (cons (copy-sequence types) query))
        query))))

;;;; Node classification -------------------------------------------------

(defun thb-markdown-decor--child-of-type (parent type)
  "First child of PARENT whose `treesit-node-type' equals TYPE."
  (let ((i 0)
        (n (treesit-node-child-count parent))
        found)
    (while (and (null found) (< i n))
      (let ((child (treesit-node-child parent i)))
        (when (equal (treesit-node-type child) type)
          (setq found child)))
      (setq i (1+ i)))
    found))

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

;;;; Text-property primitives -------------------------------------------

(defun thb-markdown-decor--hide-range (start end)
  "Mark [START, END) invisible under our spec symbol.
Idempotent; calling twice is fine."
  (when (< start end)
    (put-text-property start end 'invisible
                       thb-markdown-decor--invisibility-symbol)))

(defconst thb-markdown-decor--literal-types '("code_span" "link_destination")
  "Node types whose contents must never be touched by delimiter hiding.")

(defconst thb-markdown-decor--container-types
  '("emphasis" "strong_emphasis" "strikethrough")
  "Node types whose delimiters `thb-markdown-decor--hide-delimiters' hides.")

(defun thb-markdown-decor--hide-delimiters (node type)
  "Hide TYPE delimiter tokens under NODE.

Descends the subtree rather than looking only at direct children: the
grammar nests some containers, so `~~x~~' parses as

  (strikethrough (delim) (strikethrough (delim) (delim)) (delim))

with one tilde per level, and a direct-children-only pass would hide half
the tildes.  Never descends into `code_span' or `link_destination', whose
contents have to stay literal, nor into a nested container of another
emphasis type: that node is captured on its own, so its own policy (exact
~~double~~ strikethrough) and `thb-markdown-decor-decorated-types' decide."
  (let ((i 0) (n (treesit-node-child-count node))
        (own (treesit-node-type node)))
    (while (< i n)
      (let* ((child (treesit-node-child node i))
             (ctype (treesit-node-type child)))
        (cond
         ((equal ctype type)
          (thb-markdown-decor--hide-range
           (treesit-node-start child) (treesit-node-end child)))
         ((member ctype thb-markdown-decor--literal-types) nil)
         ((and (member ctype thb-markdown-decor--container-types)
               (not (equal ctype own)))
          nil)
         (t (thb-markdown-decor--hide-delimiters child type))))
      (setq i (1+ i)))))

(defun thb-markdown-decor--decorate-node (node)
  "Apply decoration text-properties to NODE.
NODE is assumed to have passed `thb-markdown-decor--decoratable-node-p'."
  (let ((type (treesit-node-type node)))
    (cond
     ;; Emphasis-family containers: hide delimiter children.
     ((member type '("emphasis" "strong_emphasis" "strikethrough"))
      (thb-markdown-decor--hide-delimiters node "emphasis_delimiter"))
     ((equal type "code_span")
      (thb-markdown-decor--hide-delimiters node "code_span_delimiter"))
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
            (when (and dest thb-markdown-decor-show-link-url-echo (< ts te))
              (let ((url-text (buffer-substring-no-properties
                               (treesit-node-start dest)
                               (treesit-node-end   dest))))
                ;; Don't clobber a `help-echo' somebody else already owns
                ;; here; we cannot restore it on clear.
                (unless (text-property-not-all ts te 'help-echo nil)
                  (put-text-property ts te 'help-echo url-text))
                (put-text-property ts te 'thb-markdown-decor-url
                                   url-text))))))))))

(defun thb-markdown-decor--clear-range (start end)
  "Remove our invisibility + help-echo properties in [START, END]."
  (with-silent-modifications
    (let ((pos start))
      (while (< pos end)
        (let ((next (next-single-property-change pos 'invisible nil end)))
          ;; Only clear `invisible' where the value is our symbol; don't
          ;; trample on other minor modes that may set it.  Remove the
          ;; property outright rather than setting it to nil, so cleared
          ;; text really is back to its original property set.
          (when (eq (get-text-property pos 'invisible)
                    thb-markdown-decor--invisibility-symbol)
            (remove-text-properties pos next '(invisible nil)))
          (setq pos next))))
    (let ((pos start))
      (while (< pos end)
        (let* ((next (next-single-property-change
                      pos 'thb-markdown-decor-url nil end))
               (url (get-text-property pos 'thb-markdown-decor-url)))
          (when url
            ;; Only drop `help-echo' if it is still the one we installed.
            (when (equal (get-text-property pos 'help-echo) url)
              (remove-text-properties pos next '(help-echo nil)))
            (remove-text-properties pos next '(thb-markdown-decor-url nil)))
          (setq pos next))))))

;;;; Decoration ----------------------------------------------------------

(defun thb-markdown-decor--region-bounds (parsers start end)
  "Expand [START, END] to whole inline ranges of PARSERS.
Returns a cons (BEG . END).  Inline nodes routinely span several lines
inside one paragraph, so re-decorating only the changed line would leave
the delimiters on the other lines stale."
  (let ((beg start) (fin end))
    (dolist (parser parsers)
      (dolist (range (treesit-parser-included-ranges parser))
        ;; Overlap is tested against the ORIGINAL region so the expansion
        ;; can't snowball across the buffer.
        (when (and (< (car range) end) (> (cdr range) start))
          (setq beg (min beg (car range))
                fin (max fin (cdr range))))))
    (cons beg fin)))

(defun thb-markdown-decor--decorate-region (start end)
  "Re-decorate the inline ranges overlapping [START, END].
Returns the cons (BEG . END) actually touched."
  (let* ((parsers (thb-markdown-decor--inline-parsers-in-range start end))
         (bounds  (thb-markdown-decor--region-bounds parsers start end))
         (beg (car bounds))
         (fin (cdr bounds))
         (query (thb-markdown-decor--query)))
    (with-silent-modifications
      (thb-markdown-decor--clear-range beg fin)
      (dolist (parser parsers)
        (dolist (node (treesit-query-capture
                       (treesit-parser-root-node parser) query beg fin t))
          (when (thb-markdown-decor--decoratable-node-p node)
            (thb-markdown-decor--decorate-node node))))
      ;; A node the cursor is sitting inside must stay revealed even though
      ;; we just re-hid everything in this region.
      (thb-markdown-decor--reassert-reveal beg fin))
    bounds))

(defun thb-markdown-decor--fontify (start end)
  "`jit-lock' entry point: decorate the inline ranges around [START, END]."
  (when thb-markdown-decor-mode
    (with-demoted-errors "thb-markdown-decor: %S"
      (let ((bounds (thb-markdown-decor--decorate-region start end)))
        ;; Tell jit-lock how much we actually touched.
        (cons 'jit-lock-bounds bounds)))))

(defun thb-markdown-decor--clear ()
  "Remove all our decoration text-properties from the buffer."
  (save-restriction
    (widen)
    (thb-markdown-decor--clear-range (point-min) (point-max))))

;;;; Cursor reveal ------------------------------------------------------

(defun thb-markdown-decor--forget-revealed ()
  "Drop the revealed-range markers without re-decorating."
  (when thb-markdown-decor--revealed
    (set-marker (car thb-markdown-decor--revealed) nil)
    (set-marker (cdr thb-markdown-decor--revealed) nil)
    (setq thb-markdown-decor--revealed nil)))

(defun thb-markdown-decor--revealed-bounds ()
  "Return (START . END) of the revealed range, or nil."
  (when-let* ((r thb-markdown-decor--revealed)
              (start (marker-position (car r)))
              (end   (marker-position (cdr r))))
    (and (< start end) (cons start end))))

(defun thb-markdown-decor--reassert-reveal (start end)
  "Re-hide nothing inside the revealed range if it overlaps [START, END]."
  (when-let* ((bounds (thb-markdown-decor--revealed-bounds)))
    (when (and (< (car bounds) end) (> (cdr bounds) start))
      (thb-markdown-decor--clear-range (car bounds) (cdr bounds)))))

(defun thb-markdown-decor--decorated-parent-at (pos)
  "Smallest decoratable ancestor at POS that actually contains POS.

Guard against `treesit-node-at' returning the next node when POS sits
in plain text -- in that case `treesit-parent-until' happily walks up
from a nearby node and we'd falsely reveal something the cursor isn't
actually inside.  Verify range containment before returning."
  (when-let* ((node (treesit-node-at pos 'markdown-inline))
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
    (thb-markdown-decor--forget-revealed)
    (setq thb-markdown-decor--revealed
          ;; End marker advances with text typed at the end of the node so
          ;; the recorded range keeps matching the node while you edit it.
          (cons (copy-marker start nil) (copy-marker end t)))
    ;; Echo URL for links, but never step on a message somebody else is
    ;; currently showing (eldoc, isearch, a prompt, ...).
    (when (and (equal (treesit-node-type node) "inline_link")
               thb-markdown-decor-show-link-url-echo
               (not (current-message))
               (not (active-minibuffer-window)))
      (when-let* ((dest (thb-markdown-decor--child-of-type node "link_destination")))
        (let ((url (buffer-substring-no-properties
                    (treesit-node-start dest)
                    (treesit-node-end   dest)))
              (message-log-max nil))
          (message "%s" url))))))

(defun thb-markdown-decor--rehide ()
  "Re-apply decorations over the currently-revealed range.
Widen first: point can leave a node that extends past the narrowing, and
decoration covers the node's whole inline range.  The range is unrecorded
while decorating, so `thb-markdown-decor--reassert-reveal' leaves it hidden,
and restored if decorating fails, so the node is never left shown but
untracked."
  (when-let* ((bounds (thb-markdown-decor--revealed-bounds)))
    (let ((revealed thb-markdown-decor--revealed)
          (narrowed (buffer-narrowed-p)))
      (setq thb-markdown-decor--revealed nil)
      (condition-case err
          (save-restriction
            (widen)
            ;; A node lookup made while narrowed clips the local
            ;; markdown-inline ranges to the narrowing, and the clipping
            ;; outlives `widen'; restore the full ranges before querying.
            ;; Updating ranges outdates existing nodes, so callers must look
            ;; nodes up again afterwards.
            (when narrowed
              (treesit-update-ranges (car bounds) (cdr bounds)))
            (thb-markdown-decor--decorate-region (car bounds) (cdr bounds)))
        (error (setq thb-markdown-decor--revealed revealed)
               (signal (car err) (cdr err))))
      (set-marker (car revealed) nil)
      (set-marker (cdr revealed) nil))))

(defun thb-markdown-decor--post-command ()
  "Manage reveal/rehide based on current point position.

An error here would make Emacs drop this function from
`post-command-hook' for the rest of the session, silently killing
reveal-on-entry, so demote errors instead."
  (with-demoted-errors "thb-markdown-decor: %S"
    (when thb-markdown-decor-mode
      (let ((node (thb-markdown-decor--decorated-parent-at (point)))
            (bounds (thb-markdown-decor--revealed-bounds)))
        ;; Compare against the live node rather than trusting the recorded
        ;; range: the node may have grown, shrunk or disappeared under us.
        (unless (and node bounds
                     (eql (car bounds) (treesit-node-start node))
                     (eql (cdr bounds) (treesit-node-end node)))
          (thb-markdown-decor--rehide)
          ;; Re-hiding may update parser ranges, which outdates NODE.
          (when node
            (when-let* ((live (thb-markdown-decor--decorated-parent-at (point))))
              (thb-markdown-decor--reveal live))))))))

;;;; Refresh -------------------------------------------------------------

(defun thb-markdown-decor-refresh ()
  "Clear and re-request decoration for the whole buffer.
Decoration itself stays lazy: `jit-lock' redoes the displayed parts now
and the rest as it scrolls into view."
  (interactive)
  (thb-markdown-decor--forget-revealed)
  (thb-markdown-decor--clear)
  (setq thb-markdown-decor--query-cache nil)
  (jit-lock-refontify))

;;;; Minor mode ---------------------------------------------------------

;;;###autoload
(define-minor-mode thb-markdown-decor-mode
  "Decorate markdown source in place: hide delimiters, render link text.

Intended for buffers with the markdown-inline tree-sitter parser attached
\(`markdown-ts-mode').  Applies `invisible' text-properties to:

  - emphasis / strong / strikethrough delimiter pairs
  - inline-code backticks
  - the closing tail of inline links (URL shown via help-echo)

Point-entry into a decorated node reveals its delimiters automatically;
they re-hide on cursor exit.

Decoration runs through `jit-lock', so enabling the mode is cheap even on
a large document and nothing is walked until it is displayed.  Enabling in
a buffer that has no markdown-inline parser yet is harmless: those parsers
are created lazily during fontification, and decoration simply starts
working once they exist.

Use \\[thb-markdown-decor-mode] to toggle, or auto-enable via:

  (add-hook \\='markdown-ts-mode-hook #\\='thb-markdown-decor-mode)"
  :lighter " mdr"
  :group 'thb-markdown-decor
  (cond
   (thb-markdown-decor-mode
    (setq thb-markdown-decor--query-cache nil)
    (add-to-invisibility-spec thb-markdown-decor--invisibility-symbol)
    (add-hook 'post-command-hook #'thb-markdown-decor--post-command nil t)
    (jit-lock-register #'thb-markdown-decor--fontify)
    ;; `jit-lock-register' prepends, and `add-hook' ignores the DEPTH of a
    ;; function that is already on the hook, so remove and re-add to land
    ;; after font-lock's own jit-lock entry.  Running last means our
    ;; "is `help-echo' already taken here" test sees the final answer.
    (remove-hook 'jit-lock-functions #'thb-markdown-decor--fontify t)
    (add-hook 'jit-lock-functions #'thb-markdown-decor--fontify 90 t)
    (jit-lock-refontify))
   (t
    (remove-from-invisibility-spec thb-markdown-decor--invisibility-symbol)
    (remove-hook 'post-command-hook #'thb-markdown-decor--post-command t)
    (jit-lock-unregister #'thb-markdown-decor--fontify)
    (thb-markdown-decor--forget-revealed)
    (thb-markdown-decor--clear))))

(provide 'thb-markdown-decor)
;;; thb-markdown-decor.el ends here
