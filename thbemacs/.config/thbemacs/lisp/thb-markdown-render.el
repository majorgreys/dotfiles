;;; thb-markdown-render.el --- DIY markdown -> fontified buffer renderer  -*- lexical-binding: t; -*-
;;
;; Author: Tahir Butt
;; Keywords: text, markdown, preview
;;
;;; Commentary:
;;
;; A homegrown markdown previewer that walks the tree-sitter-markdown AST
;; directly and emits fontified text into an Emacs buffer.  Replaces the
;; md2html -> shr/eww path with full control over rendering decisions.
;;
;; Why not just use shr?  shr is a character-grid HTML renderer designed
;; for displaying email and casual browsing.  It struggles with:
;;   - Proportional + monospace layout coexistence (pixel/char mismatch)
;;   - Table borders (each cell renders independently, outer + inner stack)
;;   - Code block language injection (no built-in support)
;;   - CSS-style row backgrounds, alternation, custom typography
;;
;; This renderer skips HTML entirely.  We already have the markdown AST
;; from `markdown-ts-mode' (tree-sitter-markdown + tree-sitter-markdown-
;; inline grammars), so we emit text directly with the faces and overlays
;; we want.  Currently ~v0.1 of an open-ended project; target < 1k LOC.
;;
;; Entry points:
;;
;;   M-x thb-md-render-file PATH       — render file to a fresh buffer
;;   M-x thb-md-render-toggle           — toggle between markdown source
;;                                        buffer and its rendered preview
;;
;; Coverage in v0.1:
;;   - ATX headings (H1-H6) with scaled faces
;;   - Paragraphs, with inline emphasis / strong / strikethrough / code
;;   - Fenced code blocks (language tag captured but no injection yet)
;;   - Unordered, ordered, and task lists (with Unicode glyphs)
;;   - Block quotes (with marker + bg)
;;   - Inline links (text rendered, URL via `help-echo')
;;   - Thematic breaks
;;
;; Not yet:
;;   - Setext headings
;;   - Tables (next milestone)
;;   - Indented code blocks
;;   - Inline images (would need overlays + image scaling)
;;   - Reference-style links / link reference definitions
;;   - HTML blocks
;;   - Tree-sitter language injection inside fenced code blocks
;;
;;; Code:

(require 'treesit)
(require 'cl-lib)
(require 'subr-x)

;;;; Customization -------------------------------------------------------

(defgroup thb-md-render nil
  "DIY tree-sitter-based markdown renderer."
  :group 'text
  :prefix "thb-md-render-")

(defcustom thb-md-render-heading-scales '(2.0 1.5 1.25 1.1 1.0 0.9)
  "Per-level :height multipliers for ATX heading faces.
Six values, H1 through H6.  Matches the shr-preview ladder."
  :type '(repeat number)
  :group 'thb-md-render)

(defcustom thb-md-render-blockquote-prefix "▎ "
  "String prefixed to each line inside a block quote.
The default is a left-bar glyph that approximates a CSS border-left."
  :type 'string
  :group 'thb-md-render)

(defcustom thb-md-render-bullet "• "
  "Bullet glyph used for unordered list items."
  :type 'string
  :group 'thb-md-render)

(defcustom thb-md-render-task-glyphs
  '(("unchecked" . "☐ ")
    ("checked"   . "☑ "))
  "Glyphs used for task-list checkboxes by `task_list_marker_*' subtype."
  :type '(alist :key-type string :value-type string)
  :group 'thb-md-render)

;;;; Faces ---------------------------------------------------------------

(defface thb-md-render-h1 '((t :inherit outline-1 :weight bold :height 2.0))
  "Face for H1 in the rendered preview."
  :group 'thb-md-render)
(defface thb-md-render-h2 '((t :inherit outline-2 :weight bold :height 1.5))
  "Face for H2 in the rendered preview."
  :group 'thb-md-render)
(defface thb-md-render-h3 '((t :inherit outline-3 :weight semi-bold :height 1.25))
  "Face for H3 in the rendered preview."
  :group 'thb-md-render)
(defface thb-md-render-h4 '((t :inherit outline-4 :weight semi-bold :height 1.1))
  "Face for H4 in the rendered preview."
  :group 'thb-md-render)
(defface thb-md-render-h5 '((t :inherit outline-5 :weight semi-bold))
  "Face for H5 in the rendered preview."
  :group 'thb-md-render)
(defface thb-md-render-h6 '((t :inherit outline-6 :weight semi-bold :height 0.9))
  "Face for H6 in the rendered preview."
  :group 'thb-md-render)

(defface thb-md-render-body
  '((t :inherit variable-pitch))
  "Default face for body prose (paragraphs, list item content)."
  :group 'thb-md-render)

(defface thb-md-render-emphasis
  '((t :inherit (italic variable-pitch)))
  "Face for *emphasized* text."
  :group 'thb-md-render)

(defface thb-md-render-strong
  '((t :inherit (bold variable-pitch)))
  "Face for **strong** text."
  :group 'thb-md-render)

(defface thb-md-render-strikethrough
  '((t :strike-through t :inherit variable-pitch))
  "Face for ~~struck~~ text."
  :group 'thb-md-render)

(defface thb-md-render-code-inline
  '((t :inherit (fixed-pitch font-lock-string-face)))
  "Face for `inline code`."
  :group 'thb-md-render)

(defface thb-md-render-code-block
  '((t :inherit fixed-pitch :extend t))
  "Face for fenced code block content.  Background is set by
`thb-md-render-apply-theme'."
  :group 'thb-md-render)

(defface thb-md-render-code-fence-info
  '((t :inherit font-lock-type-face))
  "Face for the language tag in a fenced code block info string."
  :group 'thb-md-render)

(defface thb-md-render-blockquote
  '((t :slant italic :extend t))
  "Face for block quote content.  Background is set by
`thb-md-render-apply-theme'."
  :group 'thb-md-render)

(defface thb-md-render-blockquote-marker
  '((t :inherit shadow))
  "Face for the left-bar prefix of block quote lines."
  :group 'thb-md-render)

(defface thb-md-render-list-marker
  '((t :inherit shadow))
  "Face for list bullets, ordered-list numbers, and task checkboxes."
  :group 'thb-md-render)

(defface thb-md-render-link-text
  '((t :inherit link))
  "Face for the visible text of an inline link or image description."
  :group 'thb-md-render)

(defface thb-md-render-link-url
  '((t :inherit (link shadow)))
  "Face for the URL of an inline link, when shown."
  :group 'thb-md-render)

(defface thb-md-render-thematic-break
  '((t :inherit shadow :extend t))
  "Face for a horizontal rule (---)."
  :group 'thb-md-render)

(defun thb-md-render-apply-theme ()
  "Reapply modus-themes-derived backgrounds to `thb-md-render-*' faces.
Should be called after a theme toggle."
  (when (featurep 'modus-themes)
    (let ((bg-code  (modus-themes-get-color-value 'bg-dim))
          (bg-quote (modus-themes-get-color-value 'bg-blue-nuanced)))
      (set-face-attribute 'thb-md-render-code-block  nil :background bg-code)
      (set-face-attribute 'thb-md-render-blockquote  nil :background bg-quote))))

(with-eval-after-load 'modus-themes
  (thb-md-render-apply-theme))

;;;; Source-buffer state ------------------------------------------------

(defvar thb-md-render--src-buffer nil
  "Dynamic binding: source buffer the parsers are attached to.
Bound by `thb-md-render-file' for the duration of one render.")

(defun thb-md-render--src-text (start end)
  "Return text from source buffer between START and END."
  (with-current-buffer thb-md-render--src-buffer
    (buffer-substring-no-properties start end)))

(defun thb-md-render--node-text (node)
  "Return source text of NODE."
  (thb-md-render--src-text (treesit-node-start node)
                           (treesit-node-end   node)))

(defun thb-md-render--children-of-type (node type)
  "Return direct children of NODE whose `treesit-node-type' is TYPE (a string)."
  (seq-filter (lambda (c) (equal (treesit-node-type c) type))
              (treesit-node-children node)))

(defun thb-md-render--first-child-of-type (node type)
  (car (thb-md-render--children-of-type node type)))

;;;; Emit helpers --------------------------------------------------------

(defun thb-md-render--emit (text &optional face)
  "Insert TEXT into current (output) buffer, optionally with FACE."
  (let ((start (point)))
    (insert text)
    (when face
      (put-text-property start (point) 'face face))))

(defun thb-md-render--newline (&optional n)
  "Insert N (default 1) newlines into the output buffer."
  (insert (make-string (or n 1) ?\n)))

(defun thb-md-render--ensure-blank-line ()
  "Ensure there's at least one blank line before point (block separator).
Does nothing at buffer start; collapses adjacent blank lines so we don't
accumulate them across nested blocks."
  (cond
   ((= (point) (point-min)) nil)
   ((looking-back "\n\n" 2) nil)
   ((looking-back "\n" 1)   (insert "\n"))
   (t                        (insert "\n\n"))))

;;;; Inline rendering ----------------------------------------------------

;; Inline nodes live in the markdown-inline parser's tree.  When we hit an
;; (inline) block node from the markdown grammar, we walk the inline
;; parser's parse for that range and emit text + faces.
;;
;; The inline tree for a paragraph "Hello *world* and `code`" looks like:
;;
;;   (inline
;;     (emphasis (emphasis_delimiter) (emphasis_delimiter))
;;     (code_span (code_span_delimiter) (code_span_delimiter)))
;;
;; Plain text between tokens is implicit in the source buffer between the
;; node ranges -- there are no "text" nodes.

(defvar thb-md-render--inline-face-stack nil
  "Stack of currently-active inline faces while walking inline children.
Outermost face is on top.  Used so nested emphasis inherits parent face.")

(defun thb-md-render--inline-face-for (type)
  "Return the inline face for an inline node TYPE (string), or nil."
  (pcase type
    ("emphasis"        'thb-md-render-emphasis)
    ("strong_emphasis" 'thb-md-render-strong)
    ("strikethrough"   'thb-md-render-strikethrough)
    ("code_span"       'thb-md-render-code-inline)
    ("link_text"       'thb-md-render-link-text)
    ("image_description" 'thb-md-render-link-text)
    (_ nil)))

(defun thb-md-render--emit-plain (start end)
  "Emit plain (un-tokenized) source text in the range START..END.
Applies the topmost face from `thb-md-render--inline-face-stack', or
`thb-md-render-body' if the stack is empty (top-level prose)."
  (when (< start end)
    (let* ((text (thb-md-render--src-text start end))
           (face (or (car thb-md-render--inline-face-stack)
                     'thb-md-render-body)))
      (thb-md-render--emit text face))))

(defun thb-md-render--walk-inline-range (children start end &optional skip-types)
  "Walk inline CHILDREN that lie within source range START..END.
For each non-skipped child:
- emit plain text from CURSOR to child-start (with current face stack),
- dispatch the child via `thb-md-render--emit-inline-node',
- advance CURSOR to child-end.
After all children, emit any trailing plain text up to END.
SKIP-TYPES is an optional list of node-type strings to ignore entirely
(e.g. `(\"emphasis_delimiter\" \"code_span_delimiter\")')."
  (let ((cursor start))
    (dolist (child children)
      (let ((cs   (treesit-node-start child))
            (ce   (treesit-node-end   child))
            (type (treesit-node-type  child)))
        (when (and (>= cs start) (< cs end)
                   (not (member type skip-types)))
          (thb-md-render--emit-plain cursor cs)
          (thb-md-render--emit-inline-node child)
          (setq cursor ce))))
    (thb-md-render--emit-plain cursor end)))

(defun thb-md-render--inline-walk (start end)
  "Render inline content in source range START..END.
Finds the markdown-inline parser's root and walks its direct children
that fall within the range, gap-filling plain text between them."
  (let* ((inline-parser (car (treesit-parser-list thb-md-render--src-buffer
                                                  'markdown-inline)))
         (children (when inline-parser
                     (treesit-node-children
                      (treesit-parser-root-node inline-parser)))))
    (thb-md-render--walk-inline-range children start end)))

(defun thb-md-render--emit-inline-node (node)
  "Emit a single inline parser NODE (emphasis, code_span, link, etc.)."
  (let* ((type  (treesit-node-type node))
         (face  (thb-md-render--inline-face-for type)))
    (pcase type
      ;; Containers: push face, walk children gap-filled inside delimiters.
      ;; The grammar emits one `emphasis_delimiter' node per asterisk/tilde/
      ;; underscore: `**bold**' has 4 delim children (two opening + two
      ;; closing), `*em*' has 2.  Total is always even; first half are
      ;; opening, second half are closing.  Inner content range is:
      ;;   - inner-start = end of the last opening delim
      ;;   - inner-end   = start of the first closing delim
      ((or "emphasis" "strong_emphasis" "strikethrough")
       (let* ((children (treesit-node-children node))
              (delims   (seq-filter
                         (lambda (c) (equal (treesit-node-type c) "emphasis_delimiter"))
                         children))
              (n-delims (length delims))
              (half     (/ n-delims 2))
              (inner-start (if (> n-delims 0)
                               (treesit-node-end (nth (1- half) delims))
                             (treesit-node-start node)))
              (inner-end   (if (>= n-delims 2)
                               (treesit-node-start (nth half delims))
                             (treesit-node-end node))))
         (let ((thb-md-render--inline-face-stack
                (cons face thb-md-render--inline-face-stack)))
           (thb-md-render--walk-inline-range
            children inner-start inner-end
            '("emphasis_delimiter")))))
      ;; Code span: emit content between delimiters in code-inline face.
      ("code_span"
       (let ((delims (thb-md-render--children-of-type node "code_span_delimiter")))
         (if (= (length delims) 2)
             (let ((cs (treesit-node-end (car delims)))
                   (ce (treesit-node-start (cadr delims))))
               (thb-md-render--emit (thb-md-render--src-text cs ce)
                                    'thb-md-render-code-inline))
           ;; Malformed; fall back to whole-node text.
           (thb-md-render--emit (thb-md-render--node-text node)
                                'thb-md-render-code-inline))))
      ;; Inline link [text](url): emit text, attach url as help-echo and link.
      ("inline_link"
       (let* ((text-node (thb-md-render--first-child-of-type node "link_text"))
              (dest-node (thb-md-render--first-child-of-type node "link_destination"))
              (text (and text-node
                         (let ((s (treesit-node-start text-node))
                               (e (treesit-node-end   text-node)))
                           ;; link_text contains [ ... ]; trim the brackets.
                           (thb-md-render--src-text (1+ s) (1- e)))))
              (url  (and dest-node (thb-md-render--node-text dest-node))))
         (let ((start-out (point)))
           (thb-md-render--emit (or text "") 'thb-md-render-link-text)
           (when url
             (put-text-property start-out (point) 'help-echo url)
             (put-text-property start-out (point) 'thb-md-link url)))))
      ;; Image ![alt](src): show alt + faint URL.  No inline image yet.
      ("image"
       (let* ((alt-node (thb-md-render--first-child-of-type node "image_description"))
              (dest-node (thb-md-render--first-child-of-type node "link_destination"))
              (alt (and alt-node
                        (let ((s (treesit-node-start alt-node))
                              (e (treesit-node-end   alt-node)))
                          (thb-md-render--src-text (+ s 2) (1- e)))))  ;; trim ![ and ]
              (src (and dest-node (thb-md-render--node-text dest-node))))
         (thb-md-render--emit "🖼 " 'thb-md-render-list-marker)
         (when alt (thb-md-render--emit alt 'thb-md-render-link-text))
         (when src
           (thb-md-render--emit " " 'thb-md-render-body)
           (thb-md-render--emit src 'thb-md-render-link-url))))
      ;; Shortcut link [foo]: just emit the text in link face.
      ("shortcut_link"
       (let* ((text-node (thb-md-render--first-child-of-type node "link_text"))
              (text (and text-node
                         (let ((s (treesit-node-start text-node))
                               (e (treesit-node-end   text-node)))
                           (thb-md-render--src-text (1+ s) (1- e))))))
         (thb-md-render--emit (or text "") 'thb-md-render-link-text)))
      ;; Fallthrough: just emit the source text with current face.
      (_
       (let ((start (treesit-node-start node))
             (end   (treesit-node-end   node)))
         (thb-md-render--emit-plain start end))))))

;;;; Block rendering -----------------------------------------------------

(defvar thb-md-render--list-state nil
  "Stack of (KIND . COUNTER) cells while inside lists.
KIND is `unordered' or `ordered'.  COUNTER is the next ordinal for an
ordered list (incremented per `list_item').")

(defun thb-md-render--walk (node)
  "Top-level dispatch on block-level NODE from the markdown parser."
  (pcase (treesit-node-type node)
    ((or "document" "section")
     (dolist (c (treesit-node-children node))
       (thb-md-render--walk c)))
    ("atx_heading"
     (thb-md-render--walk-heading node))
    ("paragraph"
     (thb-md-render--walk-paragraph node))
    ("fenced_code_block"
     (thb-md-render--walk-fenced-code node))
    ("indented_code_block"
     (thb-md-render--walk-indented-code node))
    ("list"
     (thb-md-render--walk-list node))
    ("block_quote"
     (thb-md-render--walk-blockquote node))
    ("thematic_break"
     (thb-md-render--walk-thematic-break))
    ("setext_heading"
     ;; Treat as H1/H2 depending on underline.
     (thb-md-render--walk-setext-heading node))
    (_
     ;; Unknown: walk children if any so we don't lose content.
     (dolist (c (treesit-node-children node))
       (thb-md-render--walk c)))))

;;;; Block: heading ------------------------------------------------------

(defun thb-md-render--atx-level (atx-heading)
  "Return 1..6 for the atx_h{N}_marker child of ATX-HEADING."
  (let ((marker (treesit-node-child atx-heading 0)))
    (cond
     ((equal (treesit-node-type marker) "atx_h1_marker") 1)
     ((equal (treesit-node-type marker) "atx_h2_marker") 2)
     ((equal (treesit-node-type marker) "atx_h3_marker") 3)
     ((equal (treesit-node-type marker) "atx_h4_marker") 4)
     ((equal (treesit-node-type marker) "atx_h5_marker") 5)
     ((equal (treesit-node-type marker) "atx_h6_marker") 6)
     (t 1))))

(defun thb-md-render--heading-face (level)
  (intern (format "thb-md-render-h%d" level)))

(defun thb-md-render--walk-heading (node)
  (thb-md-render--ensure-blank-line)
  (let* ((level (thb-md-render--atx-level node))
         (face  (thb-md-render--heading-face level))
         (content (thb-md-render--first-child-of-type node "inline")))
    (let ((thb-md-render--inline-face-stack (list face)))
      (when content
        (thb-md-render--inline-walk (treesit-node-start content)
                                    (treesit-node-end   content))))
    (thb-md-render--newline 2)))

(defun thb-md-render--walk-setext-heading (node)
  (thb-md-render--ensure-blank-line)
  (let* ((has-h1 (thb-md-render--first-child-of-type node "setext_h1_underline"))
         (level  (if has-h1 1 2))
         (face   (thb-md-render--heading-face level))
         (content (thb-md-render--first-child-of-type node "paragraph"))
         (inline (and content (thb-md-render--first-child-of-type content "inline"))))
    (let ((thb-md-render--inline-face-stack (list face)))
      (when inline
        (thb-md-render--inline-walk (treesit-node-start inline)
                                    (treesit-node-end   inline))))
    (thb-md-render--newline 2)))

;;;; Block: paragraph ----------------------------------------------------

(defun thb-md-render--walk-paragraph (node)
  (thb-md-render--ensure-blank-line)
  (let ((inline (thb-md-render--first-child-of-type node "inline")))
    (when inline
      (thb-md-render--inline-walk (treesit-node-start inline)
                                  (treesit-node-end   inline))))
  (thb-md-render--newline 2))

;;;; Block: fenced code -------------------------------------------------

(defun thb-md-render--walk-fenced-code (node)
  (thb-md-render--ensure-blank-line)
  (let* ((info-node (thb-md-render--first-child-of-type node "info_string"))
         (lang-node (and info-node
                         (thb-md-render--first-child-of-type info-node "language")))
         (content   (thb-md-render--first-child-of-type node "code_fence_content"))
         (lang      (and lang-node (thb-md-render--node-text lang-node))))
    (when lang
      (thb-md-render--emit (format "  %s\n" lang)
                           'thb-md-render-code-fence-info))
    (when content
      (let* ((raw (thb-md-render--src-text (treesit-node-start content)
                                           (treesit-node-end   content)))
             ;; Strip trailing newline so the block doesn't double-space.
             (trimmed (string-trim-right raw "\n")))
        (thb-md-render--emit (concat trimmed "\n")
                             'thb-md-render-code-block))))
  (thb-md-render--newline 1))

(defun thb-md-render--walk-indented-code (node)
  (thb-md-render--ensure-blank-line)
  (let* ((raw (thb-md-render--node-text node))
         (lines (split-string raw "\n"))
         ;; Indented code is prefixed with 4 spaces per line in source; trim.
         (cleaned (mapconcat (lambda (l)
                               (if (string-prefix-p "    " l)
                                   (substring l 4)
                                 l))
                             lines "\n"))
         (trimmed (string-trim-right cleaned "\n")))
    (thb-md-render--emit (concat trimmed "\n") 'thb-md-render-code-block))
  (thb-md-render--newline 1))

;;;; Block: list ---------------------------------------------------------

(defun thb-md-render--list-kind (list-node)
  "Return `ordered' or `unordered' for LIST-NODE based on first item's marker."
  (let* ((first-item (thb-md-render--first-child-of-type list-node "list_item")))
    (if (and first-item
             (thb-md-render--first-child-of-type first-item "list_marker_dot"))
        'ordered 'unordered)))

(defun thb-md-render--walk-list (node)
  (thb-md-render--ensure-blank-line)
  (let* ((kind (thb-md-render--list-kind node))
         (state (cons (cons kind 1) thb-md-render--list-state)))
    (let ((thb-md-render--list-state state))
      (dolist (item (thb-md-render--children-of-type node "list_item"))
        (thb-md-render--walk-list-item item))))
  ;; Items end with a single newline; ensure trailing blank line.
  (thb-md-render--newline 1))

(defun thb-md-render--walk-list-item (node)
  (let* ((kind (caar thb-md-render--list-state))
         (task (or (thb-md-render--first-child-of-type node "task_list_marker_unchecked")
                   (thb-md-render--first-child-of-type node "task_list_marker_checked")))
         (depth (1- (length thb-md-render--list-state)))
         (indent (make-string (* depth 2) ?\s))
         (prefix
          (cond
           (task
            (alist-get
             (pcase (treesit-node-type task)
               ("task_list_marker_unchecked" "unchecked")
               ("task_list_marker_checked"   "checked"))
             thb-md-render-task-glyphs
             nil nil #'equal))
           ((eq kind 'ordered)
            (let* ((n (cdar thb-md-render--list-state)))
              (setcdr (car thb-md-render--list-state) (1+ n))
              (format "%d. " n)))
           (t thb-md-render-bullet))))
    (insert indent)
    (thb-md-render--emit prefix 'thb-md-render-list-marker)
    ;; Walk item's children -- typically a paragraph + possibly nested list.
    (dolist (c (treesit-node-children node))
      (let ((type (treesit-node-type c)))
        (pcase type
          ("paragraph"
           (let ((inline (thb-md-render--first-child-of-type c "inline")))
             (when inline
               (thb-md-render--inline-walk
                (treesit-node-start inline) (treesit-node-end inline))))
           (thb-md-render--newline 1))
          ("list"
           ;; Nested list: continue at next depth.
           (thb-md-render--walk-list c))
          ;; Skip marker children (already handled).
          ((or "list_marker_minus" "list_marker_plus"
               "list_marker_star" "list_marker_dot"
               "task_list_marker_unchecked" "task_list_marker_checked"
               "block_continuation") nil)
          (_
           (thb-md-render--walk c)))))))

;;;; Block: blockquote ---------------------------------------------------

(defun thb-md-render--walk-blockquote (node)
  (thb-md-render--ensure-blank-line)
  (let ((quote-start (point)))
    ;; Walk children of the blockquote skipping markers/continuations.
    (dolist (c (treesit-node-children node))
      (let ((type (treesit-node-type c)))
        (pcase type
          ((or "block_quote_marker" "block_continuation") nil)
          (_ (thb-md-render--walk c)))))
    ;; Re-prefix every line of the rendered content with the quote glyph
    ;; and tag the whole region with the blockquote face.
    (let ((quote-end (point)))
      (save-excursion
        (goto-char quote-start)
        (while (< (point) quote-end)
          (let ((line-start (point)))
            (insert thb-md-render-blockquote-prefix)
            (put-text-property line-start (+ line-start (length thb-md-render-blockquote-prefix))
                               'face 'thb-md-render-blockquote-marker)
            (setq quote-end (+ quote-end (length thb-md-render-blockquote-prefix))))
          (forward-line 1)))
      ;; Apply blockquote background over the (now-prefixed) region.
      (font-lock-prepend-text-property quote-start quote-end
                                       'face 'thb-md-render-blockquote))))

;;;; Block: thematic break ----------------------------------------------

(defun thb-md-render--walk-thematic-break ()
  (thb-md-render--ensure-blank-line)
  (let ((start (point))
        ;; Use a thin rule that fills the window width via :extend.
        (rule (make-string 60 ?─)))
    (insert rule "\n\n")
    (put-text-property start (point) 'face 'thb-md-render-thematic-break)))

;;;; Mode for the rendered preview buffer ---------------------------------

(defvar thb-md-render-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'thb-md-render-revert)
    map)
  "Keymap for `thb-md-render-mode'.")

(define-derived-mode thb-md-render-mode special-mode "MD-Render"
  "Major mode for rendered markdown preview buffers."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (visual-line-mode 1)
  (setq-local left-fringe-width 0)
  (setq-local right-fringe-width 0)
  (when-let ((w (get-buffer-window (current-buffer))))
    (set-window-fringes w 0 0))
  (setq-local header-line-format nil)
  (display-line-numbers-mode -1)
  (buffer-disable-undo))

;;;; Entry points -------------------------------------------------------

(defvar-local thb-md-render--source-file nil
  "Path of the markdown source file rendered into the current buffer.")

(defvar-local thb-md-render--watch nil
  "`file-notify' descriptor for the source file; re-renders on change.")

(defun thb-md-render-file (path)
  "Parse PATH as markdown and render it into a fresh buffer.
Return the rendered buffer."
  (interactive "fMarkdown file: ")
  (let* ((path (expand-file-name path))
         (buf-name (format "*md render: %s*" (file-name-nondirectory path)))
         (out (get-buffer-create buf-name))
         (src (generate-new-buffer (format " *thb-md-source: %s*"
                                           (file-name-nondirectory path))
                                   t)))
    (unwind-protect
        (progn
          ;; Set up source buffer with parsers
          (with-current-buffer src
            (insert-file-contents path)
            (treesit-parser-create 'markdown)
            (treesit-parser-create 'markdown-inline)
            (setq-local treesit-range-settings
                        (treesit-range-rules
                         :embed 'markdown-inline
                         :host 'markdown
                         '((inline) @capture))))
          ;; Walk + emit
          (let ((root (treesit-parser-root-node
                       (car (treesit-parser-list src 'markdown)))))
            (with-current-buffer out
              (unless (derived-mode-p 'thb-md-render-mode)
                (thb-md-render-mode))
              (let ((inhibit-read-only t))
                (erase-buffer)
                (let ((thb-md-render--src-buffer src))
                  (thb-md-render--walk root))
                (goto-char (point-min)))
              (setq thb-md-render--source-file path))))
      (kill-buffer src))
    out))

(defun thb-md-render-revert ()
  "Re-render the current preview buffer from its source file."
  (interactive)
  (unless thb-md-render--source-file
    (user-error "Not a markdown render buffer"))
  (let ((source thb-md-render--source-file))
    (thb-md-render-file source)))

(defun thb-md-render--setup-watch (preview-buffer source-file)
  "Install a `file-notify' watch that re-renders PREVIEW-BUFFER on SOURCE-FILE change."
  (file-notify-add-watch
   source-file '(change attribute-change)
   (lambda (event)
     (condition-case err
         (pcase-let ((`(,_descriptor ,action . ,_rest) event))
           (when (buffer-live-p preview-buffer)
             (with-current-buffer preview-buffer
               (cond
                ;; Inode gone (atomic rename): schedule a re-watch + re-render.
                ((memq action '(stopped deleted renamed))
                 (run-at-time
                  0.05 nil
                  (lambda ()
                    (when (buffer-live-p preview-buffer)
                      (with-current-buffer preview-buffer
                        (when (file-readable-p thb-md-render--source-file)
                          (setq thb-md-render--watch
                                (thb-md-render--setup-watch
                                 preview-buffer thb-md-render--source-file))
                          (thb-md-render-revert)))))))
                ((memq action '(changed attribute-changed created))
                 (thb-md-render-revert))))))
       (error (message "thb-md-render watch error: %s" err))))))

(defun thb-md-render--cleanup ()
  "Remove file-notify watch when the render buffer is killed."
  (when thb-md-render--watch
    (ignore-errors (file-notify-rm-watch thb-md-render--watch))
    (setq thb-md-render--watch nil)))

(defun thb-md-render-toggle ()
  "Toggle between a markdown source buffer and its rendered preview.

In `markdown-ts-mode' -> render the file, swap to the preview buffer,
install a `file-notify' watch that re-renders on disk changes.

In an existing `thb-md-render-mode' preview -> quit back to the source
via `quit-window'."
  (interactive)
  (cond
   ((eq major-mode 'thb-md-render-mode)
    (quit-window))
   ((eq major-mode 'markdown-ts-mode)
    (let ((src (buffer-file-name)))
      (unless src (user-error "Buffer is not visiting a file"))
      (when (buffer-modified-p) (save-buffer))
      (let* ((buf-name (format "*md render: %s*" (file-name-nondirectory src)))
             (existing (get-buffer buf-name))
             (preview (thb-md-render-file src)))
        (unless (and existing (eq existing preview))
          (with-current-buffer preview
            (when thb-md-render--watch
              (ignore-errors (file-notify-rm-watch thb-md-render--watch))
              (setq thb-md-render--watch nil))
            (setq thb-md-render--watch
                  (thb-md-render--setup-watch preview src))
            (add-hook 'kill-buffer-hook #'thb-md-render--cleanup nil t)))
        (switch-to-buffer preview))))
   (t (user-error "Not in markdown-ts-mode or a render preview"))))

(provide 'thb-markdown-render)
;;; thb-markdown-render.el ends here
