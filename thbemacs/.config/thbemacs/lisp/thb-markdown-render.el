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

(defcustom thb-md-render-text-scale -1
  "Text scale steps applied to the render buffer.
Matches the semantics of `text-scale-set' / `text-scale-adjust':
  0   means \"same as the frame's default font height\";
  -1 / -2 / ... step down (smaller, each step ~1/1.2x);
  +1 / +2 / ... step up (larger).
Applies to ALL faces in the buffer, so headings and body shrink/grow
together keeping their proportional ladder.  Default -1 gives a
slightly more compact reading view than the editor default."
  :type 'integer
  :group 'thb-md-render)

(defcustom thb-md-render-body-width 140
  "Maximum body width (in characters) for the rendered preview.
Content is centered in the window with the rest as margins, the way
document viewers and reading apps do.  nil disables the constraint and
lets prose fill the full window width.
When non-nil, requires `olivetti-mode' (already declared in init.el).

The value is measured against the buffer's default face char width.
In the render buffer, default is fixed-pitch (PragmataPro) so 100
chars = ~900px.  Prose in IBM Plex Sans (narrower per char) fits
comfortably; tables in fixed-pitch align column-wise within the
same body width."
  :type '(choice (const :tag "Fill window" nil)
                 (integer :tag "Columns"))
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

;; Heading faces explicitly inherit `variable-pitch' (FIRST in the
;; inherit list so its :family wins) so headings stay proportional even
;; though the buffer default is fixed-pitch.  Outline-N is inherited for
;; the per-level color from modus-themes.  Weight is semi-bold across
;; the ladder; size carries the hierarchy.
(defface thb-md-render-h1 '((t :inherit (variable-pitch outline-1) :weight semi-bold :height 1.75))
  "Face for H1 in the rendered preview."
  :group 'thb-md-render)
(defface thb-md-render-h2 '((t :inherit (variable-pitch outline-2) :weight semi-bold :height 1.5))
  "Face for H2 in the rendered preview."
  :group 'thb-md-render)
(defface thb-md-render-h3 '((t :inherit (variable-pitch outline-3) :weight semi-bold :height 1.25))
  "Face for H3 in the rendered preview."
  :group 'thb-md-render)
(defface thb-md-render-h4 '((t :inherit (variable-pitch outline-4) :weight semi-bold :height 1.1))
  "Face for H4 in the rendered preview."
  :group 'thb-md-render)
(defface thb-md-render-h5 '((t :inherit (variable-pitch outline-5) :weight semi-bold))
  "Face for H5 in the rendered preview."
  :group 'thb-md-render)
(defface thb-md-render-h6 '((t :inherit (variable-pitch outline-6) :weight semi-bold :height 0.9))
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
  '((t :inherit (fixed-pitch font-lock-type-face) :extend t))
  "Face for the language tag line above a fenced code block.
Extends across the line so it shares the code-block background tint."
  :group 'thb-md-render)

(defface thb-md-render-blockquote
  '((t :inherit variable-pitch :slant italic :extend t))
  "Face for block quote content.  Background is set by
`thb-md-render-apply-theme'."
  :group 'thb-md-render)

(defface thb-md-render-blockquote-marker
  '((t :inherit shadow))
  "Face for the left-bar prefix of block quote lines.
Fixed-pitch (via buffer default) so the ▎ glyph aligns column-wise."
  :group 'thb-md-render)

(defface thb-md-render-list-marker
  '((t :inherit (variable-pitch shadow)))
  "Face for list bullets, ordered-list numbers, and task checkboxes.
Proportional so bullets match the prose they introduce."
  :group 'thb-md-render)

(defface thb-md-render-link-text
  '((t :inherit (variable-pitch link)))
  "Face for the visible text of an inline link or image description."
  :group 'thb-md-render)

(defface thb-md-render-link-url
  '((t :inherit (variable-pitch link shadow)))
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
  '((t :inherit (bold fixed-pitch)))
  "Face for the table header row.  Bold weight + the horizontal rule
below the header are the only visual cue; no background tint."
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
          (bg-quote (modus-themes-get-color-value 'bg-blue-nuanced)))
      (set-face-attribute 'thb-md-render-code-block nil :background bg-code)
      (set-face-attribute 'thb-md-render-blockquote nil :background bg-quote))))

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
  "Render a fenced code block as a unified card:
  - optional top blank line (with bg) for vertical padding
  - optional language tag line (small label, in code-fence-info face)
  - the code itself, each line prefixed with `thb-md-render-code-indent'
    for left padding, fontified by language injection if available
  - bottom blank line (with bg)

All lines share `thb-md-render-code-block' so the bg-dim background
reads as a continuous card rather than a strip behind the code only."
  (thb-md-render--ensure-blank-line)
  (let* ((info-node (thb-md-render--first-child-of-type node "info_string"))
         (lang-node (and info-node
                         (thb-md-render--first-child-of-type info-node "language")))
         (content   (thb-md-render--first-child-of-type node "code_fence_content"))
         (lang      (and lang-node (thb-md-render--node-text lang-node)))
         (indent    "  "))
    (let ((block-start (point)))
      ;; Top padding line.
      (insert "\n")
      ;; Optional language label, indented.
      (when lang
        (insert indent)
        (let ((label-start (point)))
          (insert lang "\n")
          (put-text-property label-start (point) 'face
                             'thb-md-render-code-fence-info)))
      ;; Content, indented per line.
      (when content
        (let* ((raw (thb-md-render--src-text (treesit-node-start content)
                                             (treesit-node-end   content)))
               (trimmed (string-trim-right raw "\n"))
               (fontified (if (and lang (> (length trimmed) 0))
                              (thb-md-render--fontify-code trimmed lang)
                            trimmed))
               (content-start (point)))
          ;; Insert the (potentially fontified) content one line at a time
          ;; so each line gets the left-padding indent.  Text properties on
          ;; `fontified' are preserved by `insert' so per-token faces
          ;; survive the line splitting.
          (let ((lines (split-string fontified "\n")))
            (dolist (line lines)
              (insert indent line "\n")))
          ;; Apply code-block face on the WHOLE content range; append so
          ;; per-token faces (foreground) win for color while code-block
          ;; fills bg + fixed-pitch.
          (font-lock-append-text-property content-start (point)
                                          'face 'thb-md-render-code-block)))
      ;; Bottom padding line.
      (insert "\n")
      ;; Apply code-block face to the top padding line + (if no content,
      ;; ensure the bottom padding line is faced too).  The middle range
      ;; was already faced above; this catches the bookends.
      (font-lock-append-text-property block-start (point)
                                      'face 'thb-md-render-code-block)
      ;; Code blocks are never prose-wrapped: each line extends right.
      (thb-md-render--mark-nowrap block-start (point))))
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
         (trimmed (string-trim-right cleaned "\n"))
         (code-start (point)))
    (thb-md-render--emit (concat trimmed "\n") 'thb-md-render-code-block)
    ;; Indented code is never prose-wrapped: each line extends right.
    (thb-md-render--mark-nowrap code-start (point)))
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
;; columns align visually with monospace.  Each cell's inline content is
;; rendered to a propertized string first (pass 1), so column widths are
;; measured from the *rendered* display width -- not the source markdown
;; (which over-counts `[text](url)' links and `code' backticks).  Each
;; column is sized to its widest rendered cell (pass 2) with no width cap:
;; a wide table keeps its natural width and extends off to the right, so
;; the reader scrolls horizontally instead of the table folding into a
;; multi-line mess (the preview buffer truncates long lines -- see the
;; mode setup).  Header row is bold; a horizontal rule sits below the
;; header; body rows are plain.  No vertical separators — they cause
;; alignment issues with our shr fix and weren't needed here either.
;;
;; GFM cell alignment is encoded in the delimiter row's cell text:
;;   `:---'   left   (default)
;;   `---:'   right
;;   `:---:'  center
;;   `---'    default (left)
;; We parse those into a vector of alignment symbols and pad cells
;; accordingly when emitting.

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

(defun thb-md-render--table-render-cell (cell)
  "Render CELL's inline content to a trimmed, propertized string.
Walks the markdown-inline parse for CELL's source range (advancing the
shared inline cursor) into a scratch buffer, so emphasis / code-spans /
links survive as text properties on the returned string.  Width
measurement and wrapping then operate on the rendered text rather than
the raw source markdown.  Cells must be visited in source order so the
monotonic inline cursor stays aligned with the inline parse."
  (with-temp-buffer
    (thb-md-render--inline-walk (treesit-node-start cell)
                                (treesit-node-end   cell))
    (string-trim (buffer-string))))

(defun thb-md-render--table-natural-widths (rendered n-cols)
  "Return a vector of the max rendered display width per column.
RENDERED is a list of (CELL-VECTOR . FACE) pairs of propertized strings."
  (let ((widths (make-vector n-cols 0)))
    (dolist (row rendered)
      (let ((vec (car row)))
        (dotimes (i n-cols)
          (let ((w (string-width (aref vec i))))
            (when (> w (aref widths i)) (aset widths i w))))))
    widths))

(defun thb-md-render--table-insert-aligned (s width align last)
  "Insert string S aligned within WIDTH display columns per ALIGN.
Non-LAST columns are padded to the full WIDTH so the next column lines up;
the LAST column omits trailing padding (for left / center) so rows don't
carry a long tail of trailing spaces out to the right."
  (let* ((w   (string-width s))
         (pad (max 0 (- width w))))
    (pcase align
      ('right
       (insert (make-string pad ?\s))
       (insert s))
      ('center
       (let* ((l (/ pad 2)) (r (- pad l)))
         (insert (make-string l ?\s))
         (insert s)
         (unless last (insert (make-string r ?\s)))))
      (_  ; left / default
       (insert s)
       (unless last (insert (make-string pad ?\s)))))))

(defun thb-md-render--table-emit-row (cells face widths aligns n-cols)
  "Emit one table row from CELLS (a vector of propertized strings).
The whole row goes on a SINGLE logical line -- each cell aligned/padded to
its column width, columns separated by two spaces -- so a wide table
extends off to the right and scrolls horizontally rather than folding.
FACE, when non-nil, is prepended over the row (e.g. the header's bold)."
  (let ((row-start (point)))
    (dotimes (i n-cols)
      (let ((last (= i (1- n-cols))))
        (thb-md-render--table-insert-aligned
         (or (aref cells i) "") (aref widths i) (aref aligns i) last)
        (unless last (insert "  "))))
    (insert "\n")
    ;; Apply table-content face over the whole row (fixed-pitch); PREPEND so
    ;; the table's fixed-pitch family wins over each cell's body face (which
    ;; inherits variable-pitch).  Cell foregrounds (code-inline, link, etc.)
    ;; still come through because thb-md-render-table sets no foreground --
    ;; attribute lookup falls through to the per-cell face.
    (font-lock-prepend-text-property row-start (point)
                                     'face 'thb-md-render-table)
    (when face
      (font-lock-prepend-text-property row-start (point)
                                       'face face))))

(defun thb-md-render--walk-pipe-table (node)
  (thb-md-render--ensure-blank-line)
  (let* ((rows   (thb-md-render--table-rows node))
         (n-cols (if rows (length (car (car rows))) 0)))
    (when (> n-cols 0)
      (let* ((aligns (thb-md-render--table-alignments node n-cols))
             ;; Pass 1: render each cell to a propertized string, visiting
             ;; cells in source order so the shared inline cursor stays in
             ;; sync with the markdown-inline parse.
             (rendered
              (mapcar
               (lambda (row)
                 (let ((vec (make-vector n-cols ""))
                       (i 0))
                   (dolist (cell (car row))
                     (when (< i n-cols)
                       (aset vec i (thb-md-render--table-render-cell cell)))
                     (cl-incf i))
                   (cons vec (cdr row))))
               rows))
             ;; Pass 2: column widths = max rendered width per column.  No
             ;; budget cap -- a wide table keeps its natural width and runs
             ;; off to the right; the preview buffer truncates long lines so
             ;; the reader scrolls horizontally instead of the table folding.
             (widths (thb-md-render--table-natural-widths rendered n-cols))
             (total-width (+ (cl-loop for w across widths sum w)
                             (* 2 (1- n-cols))))
             (table-start (point)))
        ;; Pass 3: emit each row on one line, with a horizontal rule under
        ;; the header.
        (dolist (rrow rendered)
          (thb-md-render--table-emit-row (car rrow) (cdr rrow)
                                         widths aligns n-cols)
          (when (eq (cdr rrow) 'thb-md-render-table-header)
            (let ((rule-start (point)))
              (insert (make-string total-width ?─) "\n")
              (put-text-property rule-start (point) 'face
                                 'thb-md-render-table-rule))))
        ;; Tables are never prose-wrapped: each row stays on one logical
        ;; line so the whole table extends to the right and scrolls.
        (thb-md-render--mark-nowrap table-start (point)))))
  (thb-md-render--newline 1))

;;;; Block: thematic break ----------------------------------------------

(defun thb-md-render--walk-thematic-break ()
  (thb-md-render--ensure-blank-line)
  (let ((start (point))
        ;; Use a thin rule that fills the window width via :extend.
        (rule (make-string 60 ?─)))
    (insert rule "\n\n")
    (put-text-property start (point) 'face 'thb-md-render-thematic-break)))

;;;; Prose soft-wrap (renderer-owned) ----------------------------------

;; The preview buffer sets `truncate-lines' so tables and code blocks stay
;; on one logical line and extend to the right (scroll horizontally) rather
;; than folding.  Emacs has no per-line truncation, so to keep PROSE
;; readable we wrap it here, at render time, into real lines.  The wrap is
;; pixel-accurate (`string-pixel-width') against the olivetti body width, so
;; variable-pitch prose breaks where visual-line-mode would have.  Tables /
;; code blocks are tagged `thb-md-render--nowrap' and skipped.

(defun thb-md-render--mark-nowrap (start end)
  "Tag START..END so `thb-md-render--rewrap-prose' leaves it on one line."
  (when (< start end)
    (put-text-property start end 'thb-md-render--nowrap t)))

(defun thb-md-render--prose-budget-px ()
  "Pixel width prose should wrap to: the olivetti body width, in pixels.
Scale-invariant -- text-scale multiplies the body width and the prose font
equally, so measure at the frame's base default char width.  If the buffer
is shown in a window narrower than the body, wrap to the window instead."
  (let* ((cols (if (and (numberp thb-md-render-body-width)
                        (> thb-md-render-body-width 0))
                   thb-md-render-body-width 80))
         (win  (get-buffer-window (current-buffer) t))
         (wcols (and win (window-body-width win)))
         (eff  (if (and wcols (> wcols 0)) (min cols wcols) cols)))
    (* eff (frame-char-width))))

(defun thb-md-render--split-words (s)
  "Split S into a list of non-space substrings (text properties preserved)."
  (let ((words nil) (i 0) (n (length s)))
    (while (< i n)
      (while (and (< i n) (eq (aref s i) ?\s)) (cl-incf i))
      (let ((start i))
        (while (and (< i n) (not (eq (aref s i) ?\s))) (cl-incf i))
        (when (> i start) (push (substring s start i) words))))
    (nreverse words)))

(defun thb-md-render--sep-space (ref)
  "Return a single space carrying REF's leading `face'.
Keeps backgrounds (e.g. the blockquote tint) continuous across the word
gap the wrapper inserts between two cells of content."
  (let ((f (and (> (length ref) 0) (get-text-property 0 'face ref))))
    (if f (propertize " " 'face f) " ")))

(defun thb-md-render--pixel-wrap (s budget cont-prefix)
  "Word-wrap propertized string S to pixel width <= BUDGET.
Line 1 keeps S's own leading indent; continuation lines are prefixed with
CONT-PREFIX (the line's hang indent).  Returns one propertized string with
embedded newlines.  Breaks at ASCII spaces; a single over-long word is left
to overflow (it simply truncates off the right).  Properties are preserved."
  (let* ((lead-len (or (string-match "[^ ]" s) (length s)))
         (lead  (substring s 0 lead-len))
         (words (thb-md-render--split-words (substring s lead-len))))
    (if (null words)
        s
      (let ((lines nil) (cur lead) (has nil))
        (dolist (w words)
          (let ((cand (if has
                          (concat cur (thb-md-render--sep-space w) w)
                        (concat cur w))))
            (if (<= (string-pixel-width cand) budget)
                (setq cur cand has t)
              (if has
                  (progn (push cur lines)
                         (setq cur (concat cont-prefix w) has t))
                ;; No word placed yet on this line: keep it (will overflow).
                (setq cur cand has t)))))
        (push cur lines)
        (mapconcat #'identity (nreverse lines) "\n")))))

(defun thb-md-render--rewrap-prose ()
  "Hard-wrap prose lines to the body width so they read fine in a buffer
that truncates long lines.  Lines tagged `thb-md-render--nowrap' (tables,
code blocks) and blank lines are left alone, so they keep extending to the
right.  Continuation lines inherit each line's `wrap-prefix' as hang indent.
Run once, after the whole document has been emitted."
  (let ((budget (thb-md-render--prose-budget-px)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((bol (line-beginning-position))
              (eol (line-end-position)))
          (if (or (= bol eol)
                  (get-text-property bol 'thb-md-render--nowrap))
              (goto-char eol)
            (let ((line (buffer-substring bol eol)))
              (if (<= (string-pixel-width line) budget)
                  (goto-char eol)
                ;; The hang-indent `wrap-prefix' is set over the item's
                ;; CONTENT (after the bullet / number), not at bol, so read
                ;; it from the first position on the line that carries it.
                (let* ((prefix  (or (get-text-property bol 'wrap-prefix)
                                    (let ((p (next-single-property-change
                                              bol 'wrap-prefix nil eol)))
                                      (and p (< p eol)
                                           (get-text-property p 'wrap-prefix)))
                                    ""))
                       (wrapped (thb-md-render--pixel-wrap line budget prefix)))
                  (delete-region bol eol)
                  (goto-char bol)
                  (insert wrapped))))))
        (forward-line 1)))))

;;;; Mode for the rendered preview buffer ---------------------------------

(defvar thb-md-render-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'thb-md-render-revert)
    map)
  "Keymap for `thb-md-render-mode'.")

;; Render buffers are read-only views; evil's default `normal' state binds
;; `q' to `evil-record-macro' which shadows the mode-map's `q -> quit-window'.
;; `motion' state has the same navigation keys but no insert/record entries,
;; so `q' in motion falls through to our local binding.
(with-eval-after-load 'evil
  (evil-set-initial-state 'thb-md-render-mode 'motion))

(define-derived-mode thb-md-render-mode special-mode "MD-Render"
  "Major mode for rendered markdown preview buffers."
  ;; Truncate long logical lines (no visual wrapping).  Tables and code
  ;; blocks deliberately stay on one logical line each, so they extend to
  ;; the right and scroll horizontally instead of folding.  PROSE is wrapped
  ;; into real lines by `thb-md-render--rewrap-prose' at render time (Emacs
  ;; has no per-line truncation), so paragraphs / lists / quotes still read
  ;; at the body width even though the buffer truncates.
  (setq-local truncate-lines t)
  (setq-local word-wrap nil)
  (visual-line-mode -1)
  (setq-local left-fringe-width 0)
  (setq-local right-fringe-width 0)
  (when-let ((w (get-buffer-window (current-buffer))))
    (set-window-fringes w 0 0))
  (setq-local header-line-format nil)
  (display-line-numbers-mode -1)
  ;; Buffer default stays fixed-pitch (PragmataPro from the user's global
  ;; default) so olivetti's char-width measurement matches what tables
  ;; render in.  Without this, olivetti measures based on the variable-
  ;; pitch font (narrower per char) and tables in fixed-pitch overflow
  ;; the body width, getting visually wrapped by visual-line-mode.
  ;;
  ;; Prose faces -- body, headings, blockquote, list-marker, link-text
  ;; -- explicitly :inherit variable-pitch instead of relying on a
  ;; variable-pitch-mode default remap.  Code/table/rule faces inherit
  ;; fixed-pitch.  Both align with the buffer default for olivetti's
  ;; sake.

  ;; Apply the configured text scale.  Affects all faces uniformly, so
  ;; the heading ladder stays proportional.
  (when (and (numberp thb-md-render-text-scale)
             (not (zerop thb-md-render-text-scale)))
    (text-scale-set thb-md-render-text-scale))
  ;; olivetti-reset-window (called on every window-config change before
  ;; margins are reapplied) resets fringes to the global `fringe-mode',
  ;; which clobbers our buffer-local 0/0 setting.  Set fringe-mode
  ;; buffer-locally to 0 so olivetti's reset path sets fringes to 0/0.
  (setq-local fringe-mode 0)
  ;; Constrain body to a comfortable reading width and center it in the
  ;; window, the way a document viewer would.  Olivetti handles margins
  ;; and follows window-size changes.  When the window is narrower than
  ;; the configured width, olivetti gracefully falls back to filling.
  (when (and thb-md-render-body-width
             (fboundp 'olivetti-mode))
    (setq-local olivetti-body-width thb-md-render-body-width)
    ;; Floor at (body-width - 20) or 100, whichever is higher.  Keeps
    ;; a generous minimum even on narrower windows; user can override
    ;; via `M-x customize-variable olivetti-minimum-body-width' or
    ;; setq-local in a hook.
    (setq-local olivetti-minimum-body-width
                (max 100 (- thb-md-render-body-width 20)))
    (setq-local olivetti-style nil)
    (olivetti-mode 1))
  ;; olivetti-mode-on-hook turns on `visual-line-mode' in this config, which
  ;; flips `truncate-lines' back off and soft-wraps every long line.  Re-assert
  ;; truncation AFTER olivetti has run so tables and code stay on one logical
  ;; line and extend off to the right (scroll horizontally) instead of folding.
  (visual-line-mode -1)
  (setq-local truncate-lines t)
  (setq-local word-wrap nil)
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
                ;; Wrap prose to the body width (tables / code are skipped
                ;; via `thb-md-render--nowrap'), so prose reads fine while
                ;; the buffer truncates the long table / code lines.
                (thb-md-render--rewrap-prose)
                (goto-char (point-min)))
              (setq thb-md-render--source-file path)
              ;; Re-assert no-wrap on every render, not just on fresh
              ;; mode init: an already-open preview re-rendered via revert /
              ;; file-watch must also keep wide tables (and code) on one
              ;; logical line so they extend right and scroll horizontally.
              ;; (olivetti-mode-on-hook enables `visual-line-mode', which
              ;; only runs at mode init, so the stale value can linger.)
              (when (bound-and-true-p visual-line-mode) (visual-line-mode -1))
              (setq-local truncate-lines t)
              (setq-local word-wrap nil))))
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
        (switch-to-buffer preview)
        ;; Defensive: re-apply visual tweaks AFTER the buffer is shown
        ;; in a window.  Mode setup runs before the buffer has a
        ;; window, so `set-window-fringes' and global-mode hooks can
        ;; race and reset what mode init tried to set.
        (when-let ((win (selected-window)))
          (set-window-fringes win 0 0))
        (display-line-numbers-mode -1))))
   (t (user-error "Not in markdown-ts-mode or a render preview"))))

(provide 'thb-markdown-render)
;;; thb-markdown-render.el ends here
