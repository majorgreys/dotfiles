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

(defcustom thb-md-render-language-mode-alist
  '(("python"     . python-mode)
    ("py"         . python-mode)
    ("js"         . js-mode)
    ("javascript" . js-mode)
    ("jsx"        . js-mode)
    ("ts"         . typescript-mode)
    ("typescript" . typescript-mode)
    ("tsx"        . typescript-mode)
    ("go"         . go-mode)
    ("rust"       . rust-mode)
    ("rs"         . rust-mode)
    ("c"          . c-mode)
    ("cpp"        . c++-mode)
    ("c++"        . c++-mode)
    ("java"       . java-mode)
    ("ruby"       . ruby-mode)
    ("rb"         . ruby-mode)
    ("json"       . js-mode) ; close enough for keyword/string fontification
    ("yaml"       . conf-mode)
    ("yml"        . conf-mode)
    ("toml"       . conf-mode)
    ("sh"         . sh-mode)
    ("bash"       . sh-mode)
    ("shell"      . sh-mode)
    ("zsh"        . sh-mode)
    ("css"        . css-mode)
    ("html"       . html-mode)
    ("xml"        . sgml-mode)
    ("dockerfile" . conf-mode)
    ("elisp"      . emacs-lisp-mode)
    ("emacs-lisp" . emacs-lisp-mode)
    ("lisp"       . lisp-mode)
    ("scheme"     . scheme-mode)
    ("sql"        . sql-mode)
    ("diff"       . diff-mode)
    ("patch"      . diff-mode)
    ("md"         . markdown-ts-mode)
    ("markdown"   . markdown-ts-mode))
  "Map fence info-string language to major mode for syntax fontification.
Used by `thb-md-render--fontify-code'.  Modes are tried in a cached
temp buffer; failures (missing mode, init error) fall back to no
highlighting (plain code-block face only).

We deliberately use traditional (non-tree-sitter) modes here because
`*-ts-mode' modes require external tree-sitter grammars that may not
be installed.  Traditional modes ship with Emacs and rely only on
font-lock-keywords which is always available."
  :type '(alist :key-type string :value-type symbol)
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
  '((t :inherit (fixed-pitch font-lock-type-face)))
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
  '((t :inherit (fixed-pitch shadow) :extend t))
  "Face for a horizontal rule (---).
Fixed-pitch so the box-drawing rule character renders consistently."
  :group 'thb-md-render)

(defface thb-md-render-table
  '((t :inherit fixed-pitch))
  "Base face for table content.  Fixed-pitch so columns align visually."
  :group 'thb-md-render)

(defface thb-md-render-table-header
  '((t :inherit (bold fixed-pitch) :extend t))
  "Face for the table header row.  Background is set by
`thb-md-render-apply-theme'."
  :group 'thb-md-render)

(defface thb-md-render-table-rule
  '((t :inherit (fixed-pitch shadow) :extend t))
  "Face for the horizontal rule under the table header."
  :group 'thb-md-render)

(defun thb-md-render-apply-theme ()
  "Reapply modus-themes-derived backgrounds to `thb-md-render-*' faces.
Should be called after a theme toggle."
  (when (featurep 'modus-themes)
    (let ((bg-code  (modus-themes-get-color-value 'bg-dim))
          (bg-quote (modus-themes-get-color-value 'bg-blue-nuanced))
          (bg-th    (modus-themes-get-color-value 'bg-inactive)))
      (set-face-attribute 'thb-md-render-code-block    nil :background bg-code)
      (set-face-attribute 'thb-md-render-blockquote    nil :background bg-quote)
      (set-face-attribute 'thb-md-render-table-header  nil :background bg-th))))

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

(defvar thb-md-render--inline-children nil
  "Vector of the inline parser root's children, cached for the duration
of one render.  Bound by `thb-md-render-file' so per-paragraph
`--inline-walk' calls don't re-fetch and re-filter the whole list.")

(defvar thb-md-render--inline-cursor 0
  "Monotonic index into `thb-md-render--inline-children'.
Block-level walking proceeds top-to-bottom and inline children are in
source order, so the cursor never needs to rewind.  Each call to
`--inline-walk' advances it past the consumed range.")

(defvar thb-md-render--inline-children-len 0
  "Cached `length' of `thb-md-render--inline-children' (vector).")

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
Uses the cached `thb-md-render--inline-children' vector and the
monotonic `thb-md-render--inline-cursor' to walk only the children
that fall within this range.  Total work across one render is O(total
inline children), not O(paragraphs * total inline children)."
  (let ((cursor start)
        (children thb-md-render--inline-children)
        (len      thb-md-render--inline-children-len))
    ;; Skip past any cached children entirely before this range (defensive;
    ;; in normal top-down order this loop runs zero times).
    (while (and (< thb-md-render--inline-cursor len)
                (< (treesit-node-start
                    (aref children thb-md-render--inline-cursor))
                   start))
      (cl-incf thb-md-render--inline-cursor))
    ;; Walk children whose start falls within [start, end).
    (while (and (< thb-md-render--inline-cursor len)
                (< (treesit-node-start
                    (aref children thb-md-render--inline-cursor))
                   end))
      (let* ((child (aref children thb-md-render--inline-cursor))
             (cs (treesit-node-start child))
             (ce (treesit-node-end   child)))
        (thb-md-render--emit-plain cursor cs)
        (thb-md-render--emit-inline-node child)
        (setq cursor ce)
        (cl-incf thb-md-render--inline-cursor)))
    ;; Trailing plain text up to the range end.
    (thb-md-render--emit-plain cursor end)))

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
    ("pipe_table"
     (thb-md-render--walk-pipe-table node))
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

(defvar thb-md-render--fontify-buffers (make-hash-table :test 'eq)
  "Cache of per-mode hidden buffers used for code-fence fontification.
Mode init (defining font-lock-keywords, syntax tables, etc.) is the
expensive part — cache the initialized buffer and reuse it across all
fontify calls for the same language.  Lives for the Emacs session;
buffers are killed only on `kill-emacs-hook' (via the cleanup helper).")

(defun thb-md-render--fontify-buffer-for-mode (mode)
  "Return a cached hidden buffer initialised in MODE.
Creates and primes one on first use; subsequent calls reuse it."
  (let ((cached (gethash mode thb-md-render--fontify-buffers)))
    (if (and cached (buffer-live-p cached))
        cached
      (let ((buf (generate-new-buffer
                  (format " *thb-md-fontify-cache: %s*" mode) t)))
        (with-current-buffer buf
          (delay-mode-hooks (funcall mode)))
        (puthash mode buf thb-md-render--fontify-buffers)
        buf))))

(defun thb-md-render-fontify-cleanup ()
  "Kill all cached fontification buffers.  Hooked to `kill-emacs-hook'."
  (maphash (lambda (_mode buf)
             (when (buffer-live-p buf) (kill-buffer buf)))
           thb-md-render--fontify-buffers)
  (clrhash thb-md-render--fontify-buffers))
(add-hook 'kill-emacs-hook #'thb-md-render-fontify-cleanup)

(defun thb-md-render--fontify-code (text lang)
  "Return TEXT fontified per LANG's major mode (looks up `thb-md-render-
language-mode-alist').  Returns the original TEXT unchanged on any
failure: unknown language, mode missing, mode init errors, etc.

Uses a per-mode cached buffer so mode init (font-lock-keywords compile,
syntax-table setup, ...) is paid once per language per Emacs session
rather than once per fontify call.  The returned string carries text
properties for the per-token faces font-lock applied; inserting it
preserves those properties."
  (let ((mode (cdr (assoc lang thb-md-render-language-mode-alist))))
    (if (and mode (fboundp mode))
        (condition-case _err
            (with-current-buffer
                (thb-md-render--fontify-buffer-for-mode mode)
              (let ((inhibit-read-only t))
                (erase-buffer)
                (insert text)
                (font-lock-ensure)
                (buffer-string)))
          (error text))
      text)))

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
             (trimmed (string-trim-right raw "\n"))
             ;; Inject syntax fontification when we know the language; if not,
             ;; the string comes back property-free and just gets code-block.
             (fontified (if (and lang (> (length trimmed) 0))
                            (thb-md-render--fontify-code trimmed lang)
                          trimmed))
             (start (point)))
        (insert fontified "\n")
        ;; Apply our code-block face UNDER any per-token faces font-lock set.
        ;; `font-lock-append-text-property' adds the new face at the END of
        ;; the face-list at each position, so existing per-token foregrounds
        ;; win for color while code-block fills bg + fixed-pitch elsewhere.
        (font-lock-append-text-property start (point)
                                        'face 'thb-md-render-code-block))))
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
    ;; Remember where the item's CONTENT starts; after walking children we
    ;; tag the whole content range with a `wrap-prefix' text property so
    ;; that visual continuation lines align under the text (not column 0).
    (let* ((content-start (point))
           ;; Total left padding for wraps = item indent + prefix width.
           (wrap-pad (concat indent (make-string (length prefix) ?\s))))
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
             (thb-md-render--walk c)))))
      ;; Apply wrap-prefix to the item content range.  Visual wraps of the
      ;; item's first paragraph will now hang under the text rather than
      ;; reset to column 0.  Nested list content (separate items) gets its
      ;; own wrap-prefix set by its own --walk-list-item call.
      (put-text-property content-start (point) 'wrap-prefix wrap-pad))))

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

;;;; Block: pipe table -------------------------------------------------

;; GFM tables.  Tree-sitter-markdown emits:
;;
;;   (pipe_table
;;     (pipe_table_header (pipe_table_cell)+)
;;     (pipe_table_delimiter_row (pipe_table_delimiter_cell)+)
;;     (pipe_table_row (pipe_table_cell)+)+)
;;
;; Each cell is a leaf at the markdown-grammar level; its inline content
;; (code spans, emphasis, etc.) is parsed by markdown-inline IF we extend
;; the range-restriction to include pipe_table_cell.  We do that in
;; `thb-md-render-file' so cells get tokenized like paragraph content.
;;
;; Render strategy: tables go in `thb-md-render-table' (fixed-pitch) so
;; columns align visually with monospace.  Column widths computed from
;; the longest source text per column.  Header row gets a background
;; tint via `thb-md-render-table-header'; a horizontal rule sits below
;; the header; body rows are plain.  No vertical separators — they cause
;; alignment issues with our shr fix and weren't needed here either.
;;
;; GFM cell alignment is encoded in the delimiter row's cell text:
;;   `:---'   left   (default)
;;   `---:'   right
;;   `:---:'  center
;;   `---'    default (left)
;; We parse those into a vector of alignment symbols and pad cells
;; accordingly when emitting.

(defun thb-md-render--cell-text (cell)
  "Return CELL's source text, trimmed."
  (string-trim (thb-md-render--node-text cell)))

(defun thb-md-render--table-cell-alignment (delim-cell)
  "Return `left', `center', `right', or `default' for a delimiter cell.
Parses the GFM syntax: `:---' / `:--:' / `---:' / `---' (or any length)."
  (let* ((text (string-trim (thb-md-render--node-text delim-cell)))
         (left  (and (> (length text) 0) (eq (aref text 0) ?:)))
         (right (and (> (length text) 0)
                     (eq (aref text (1- (length text))) ?:))))
    (cond
     ((and left right) 'center)
     (right            'right)
     (left             'left)
     (t                'default))))

(defun thb-md-render--table-alignments (table n-cols)
  "Return a vector of length N-COLS with per-column alignment symbols."
  (let ((delim-row (thb-md-render--first-child-of-type
                    table "pipe_table_delimiter_row"))
        (aligns (make-vector n-cols 'default)))
    (when delim-row
      (let ((cells (thb-md-render--children-of-type
                    delim-row "pipe_table_delimiter_cell"))
            (i 0))
        (dolist (c cells)
          (when (< i n-cols)
            (aset aligns i (thb-md-render--table-cell-alignment c))
            (cl-incf i)))))
    aligns))

(defun thb-md-render--table-rows (table)
  "Return a list of (CELLS . FACE) pairs for TABLE.
First element is the header row (face = thb-md-render-table-header).
Subsequent elements are body rows (face = nil)."
  (let* ((header (thb-md-render--first-child-of-type table "pipe_table_header"))
         (body   (thb-md-render--children-of-type table "pipe_table_row"))
         (rows nil))
    (when header
      (push (cons (thb-md-render--children-of-type header "pipe_table_cell")
                  'thb-md-render-table-header)
            rows))
    (dolist (r body)
      (push (cons (thb-md-render--children-of-type r "pipe_table_cell") nil) rows))
    (nreverse rows)))

(defun thb-md-render--table-col-widths (rows n-cols)
  "Return a vector of length N-COLS with the max cell-text width per column."
  (let ((widths (make-vector n-cols 0)))
    (dolist (row rows)
      (let ((i 0))
        (dolist (cell (car row))
          (when (< i n-cols)
            (let ((w (length (thb-md-render--cell-text cell))))
              (when (> w (aref widths i))
                (aset widths i w))))
          (cl-incf i))))
    widths))

(defun thb-md-render--table-emit-cell-inline (cell)
  "Emit CELL's content via the inline cursor, then return chars emitted.
Uses `thb-md-render--inline-walk' so code spans / emphasis / links inside
cells are tokenized and faced like body inline content.  The inline
parser must have been configured to parse pipe_table_cell ranges."
  (let ((start (point)))
    (thb-md-render--inline-walk (treesit-node-start cell)
                                (treesit-node-end   cell))
    ;; Trim any leading/trailing whitespace from emitted content; cells
    ;; in the source typically have a space of inner padding.
    (save-excursion
      (goto-char start)
      (when (looking-at "\\s-+")
        (delete-region start (match-end 0))))
    (save-excursion
      (goto-char (point))
      (skip-chars-backward " \t")
      (delete-region (point) (line-end-position)))
    (- (point) start)))

(defun thb-md-render--table-emit-padded-cell (cell width align)
  "Emit CELL's inline content padded to WIDTH columns per ALIGN.
ALIGN is one of `left' / `right' / `center' / `default' (= left)."
  (cond
   ((eq align 'right)
    ;; Pad before content: figure out width first by rendering into a
    ;; throwaway position, but inline-walk advances the cursor and
    ;; emits text, so we render first then move the result.  Simpler:
    ;; render, then count chars emitted, then prepend pad by inserting
    ;; at the cell's start position.
    (let* ((start (point))
           (written (thb-md-render--table-emit-cell-inline cell))
           (pad (max 0 (- width written))))
      (when (> pad 0)
        (save-excursion
          (goto-char start)
          (insert (make-string pad ?\s))))))
   ((eq align 'center)
    (let* ((start (point))
           (written (thb-md-render--table-emit-cell-inline cell))
           (pad (max 0 (- width written)))
           (left-pad (/ pad 2))
           (right-pad (- pad left-pad)))
      (when (> left-pad 0)
        (save-excursion (goto-char start) (insert (make-string left-pad ?\s))))
      (when (> right-pad 0)
        (insert (make-string right-pad ?\s)))))
   (t  ; left or default
    (let* ((written (thb-md-render--table-emit-cell-inline cell))
           (pad (max 0 (- width written))))
      (when (> pad 0)
        (insert (make-string pad ?\s)))))))

(defun thb-md-render--walk-pipe-table (node)
  (thb-md-render--ensure-blank-line)
  (let* ((rows    (thb-md-render--table-rows node))
         (header  (and rows (eq (cdr (car rows)) 'thb-md-render-table-header)
                       (car rows)))
         (n-cols  (or (and header (length (car header))) 0)))
    (when (> n-cols 0)
      (let* ((widths (thb-md-render--table-col-widths rows n-cols))
             (aligns (thb-md-render--table-alignments node n-cols))
             (total-width (+ (cl-loop for w across widths sum w)
                             (* 2 (1- n-cols)))))
        ;; Render rows.
        (dolist (row rows)
          (let ((row-start (point))
                (cells     (car row))
                (face      (cdr row))
                (i 0))
            (dolist (cell cells)
              (when (< i n-cols)
                (thb-md-render--table-emit-padded-cell
                 cell (aref widths i) (aref aligns i))
                (when (< i (1- n-cols))
                  (insert "  ")))
              (cl-incf i))
            (insert "\n")
            ;; Apply table-content face over the whole row (fixed-pitch);
            ;; layered with the per-cell faces emitted by the inline walk.
            (font-lock-append-text-property row-start (point)
                                            'face 'thb-md-render-table)
            (when face
              (font-lock-prepend-text-property row-start (point)
                                               'face face))
            ;; Horizontal rule under the header.
            (when (eq face 'thb-md-render-table-header)
              (let ((rule-start (point)))
                (insert (make-string total-width ?─) "\n")
                (put-text-property rule-start (point) 'face
                                   'thb-md-render-table-rule))))))))
  (thb-md-render--newline 1))

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
  ;; Remap default face to variable-pitch so headings, list markers, and
  ;; anything else that doesn't explicitly inherit fixed-pitch picks up
  ;; the proportional body font (IBM Plex Sans in thbemacs).  Faces that
  ;; want monospace -- code spans, code blocks, thematic break -- inherit
  ;; fixed-pitch explicitly, which overrides this remap.
  (variable-pitch-mode 1)
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
          ;; Walk + emit.  Note: we extend the inline-parser range
          ;; restriction to include pipe_table_cell so cells get tokenized
          ;; (code spans, emphasis, etc.) just like paragraph content.
          (with-current-buffer src
            (setq-local treesit-range-settings
                        (treesit-range-rules
                         :embed 'markdown-inline
                         :host 'markdown
                         '((inline) @capture
                           (pipe_table_cell) @capture))))
          (let* ((root (treesit-parser-root-node
                        (car (treesit-parser-list src 'markdown))))
                 (inline-root (treesit-parser-root-node
                               (car (treesit-parser-list src 'markdown-inline))))
                 (inline-vec (vconcat (treesit-node-children inline-root))))
            (with-current-buffer out
              (unless (derived-mode-p 'thb-md-render-mode)
                (thb-md-render-mode))
              (let ((inhibit-read-only t))
                (erase-buffer)
                (let ((thb-md-render--src-buffer src)
                      (thb-md-render--inline-children inline-vec)
                      (thb-md-render--inline-children-len (length inline-vec))
                      (thb-md-render--inline-cursor 0))
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
