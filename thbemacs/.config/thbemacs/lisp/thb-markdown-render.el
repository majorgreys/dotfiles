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
;; we want.  Currently ~v0.1 of an open-ended project.
;;
;; Entry points:
;;
;;   M-x thb-md-render-file PATH       — render file to a fresh buffer
;;   M-x thb-md-render-toggle           — toggle between markdown source
;;                                        buffer and its rendered preview
;;
;; Coverage in v0.1:
;;   - ATX and Setext headings with scaled faces
;;   - Paragraphs, with inline emphasis / strong / strikethrough / code
;;   - Fenced code blocks with language-aware syntax fontification
;;   - Indented code blocks
;;   - Unordered, ordered, and task lists (with Unicode glyphs)
;;   - Block quotes (with marker + bg) and GitHub alerts (`> [!NOTE]')
;;   - GFM pipe tables with alignment, fitted to the window
;;   - Inline and reference links: RET / mouse-1 follows them to a
;;     heading, another file's preview, a file line (#L12) or a browser
;;   - Heading navigation (n / p / u, C-c C-f / C-c C-b) and imenu
;;   - Image placeholders with alt text and source URL
;;   - Thematic breaks
;;
;; Not yet:
;;   - Footnotes
;;   - Inline image overlays and scaling
;;   - HTML blocks
;;   - Tree-sitter language injection inside fenced code blocks
;;
;;; Code:

(require 'treesit)
(require 'filenotify)
(require 'url-parse)
(require 'url-util)
(require 'xref)
(require 'font-lock)
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
In the render buffer, default is fixed-pitch (PragmataPro), so the
column count is measured in PragmataPro cells.  Prose in IBM Plex
Sans (narrower per char) fits comfortably; tables in fixed-pitch
align column-wise within the same body width."
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
;; the ladder; `thb-md-render-heading-scales' supplies the rendered size.
(defface thb-md-render-h1 '((t :inherit (variable-pitch outline-1) :weight semi-bold))
  "Face for H1 in the rendered preview."
  :group 'thb-md-render)
(defface thb-md-render-h2 '((t :inherit (variable-pitch outline-2) :weight semi-bold))
  "Face for H2 in the rendered preview."
  :group 'thb-md-render)
(defface thb-md-render-h3 '((t :inherit (variable-pitch outline-3) :weight semi-bold))
  "Face for H3 in the rendered preview."
  :group 'thb-md-render)
(defface thb-md-render-h4 '((t :inherit (variable-pitch outline-4) :weight semi-bold))
  "Face for H4 in the rendered preview."
  :group 'thb-md-render)
(defface thb-md-render-h5 '((t :inherit (variable-pitch outline-5) :weight semi-bold))
  "Face for H5 in the rendered preview."
  :group 'thb-md-render)
(defface thb-md-render-h6 '((t :inherit (variable-pitch outline-6) :weight semi-bold))
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

;; GitHub alert callouts (`> [!NOTE]' ...).  The face colours the label and
;; the quote bar; `thb-md-render-apply-theme' sets modus palette colours.
(defface thb-md-render-alert-note
  '((t :inherit (bold font-lock-function-name-face)))
  "Face for the label and bar of a NOTE alert."
  :group 'thb-md-render)
(defface thb-md-render-alert-tip
  '((t :inherit (bold success)))
  "Face for the label and bar of a TIP alert."
  :group 'thb-md-render)
(defface thb-md-render-alert-important
  '((t :inherit (bold font-lock-keyword-face)))
  "Face for the label and bar of an IMPORTANT alert."
  :group 'thb-md-render)
(defface thb-md-render-alert-warning
  '((t :inherit (bold warning)))
  "Face for the label and bar of a WARNING alert."
  :group 'thb-md-render)
(defface thb-md-render-alert-caution
  '((t :inherit (bold error)))
  "Face for the label and bar of a CAUTION alert."
  :group 'thb-md-render)

(defun thb-md-render-apply-theme (&rest _ignored)
  "Reapply modus-themes-derived colours to `thb-md-render-*' faces.
Should be called after a theme toggle.  Ignore hook arguments."
  (when (featurep 'modus-themes)
    (let ((bg-code  (modus-themes-get-color-value 'bg-dim))
          (bg-quote (modus-themes-get-color-value 'bg-blue-nuanced)))
      (set-face-attribute 'thb-md-render-code-block nil :background bg-code)
      (set-face-attribute 'thb-md-render-blockquote nil :background bg-quote)
      ;; GitHub's alert colours: blue, green, purple, yellow, red.
      (pcase-dolist (`(,face . ,color)
                     '((thb-md-render-alert-note . blue)
                       (thb-md-render-alert-tip . green)
                       (thb-md-render-alert-important . magenta)
                       (thb-md-render-alert-warning . yellow)
                       (thb-md-render-alert-caution . red)))
        (set-face-attribute face nil
                            :foreground (modus-themes-get-color-value color))))))

(with-eval-after-load 'modus-themes
  (thb-md-render-apply-theme))

;; `enable-theme-functions' runs after a theme has been enabled, when its
;; palette is available.  `add-hook' is idempotent, including when this file is
;; evaluated repeatedly in a long-lived Emacs session.
(add-hook 'enable-theme-functions #'thb-md-render-apply-theme)

;;;; Source-buffer state ------------------------------------------------

(defvar thb-md-render--src-buffer nil
  "Dynamic binding: buffer that all rendered source text is read from.
It holds the source with `block_continuation' spans blanked (see
`thb-md-render--setup-parsers') and carries the markdown-inline parser.
Bound by `thb-md-render-file' for the duration of one render.")

(defvar-local thb-md-render--source-file nil
  "Path of the markdown source file rendered into the current buffer.")

(defun thb-md-render--src-text (start end)
  "Return text from source buffer between START and END."
  (with-current-buffer thb-md-render--src-buffer
    (buffer-substring-no-properties start end)))

(defun thb-md-render--node-text (node)
  "Return source text of NODE."
  (thb-md-render--src-text (treesit-node-start node)
                           (treesit-node-end   node)))

(defun thb-md-render--setup-parsers (inline-buffer)
  "Parse the current buffer as markdown and INLINE-BUFFER as markdown-inline.
INLINE-BUFFER must hold a copy of the current buffer's text.  Every
`block_continuation' span (quote markers and list indentation on
continuation lines) is replaced there by spaces of equal length, so inline
nodes and the plain text between them never contain block markers while
positions stay identical in both buffers.

Return (MARKDOWN-PARSER . INLINE-PARSER).  INLINE-PARSER has no ranges
yet; `thb-md-render--inline-nodes' points it at one inline range at a time."
  (let* ((markdown-parser (treesit-parser-create 'markdown))
         (root (treesit-parser-root-node markdown-parser))
         (continuations
          (treesit-query-capture root '((block_continuation) @c) nil nil t)))
    (with-current-buffer inline-buffer
      (save-excursion
        (dolist (node continuations)
          (let ((start (treesit-node-start node))
                (end (treesit-node-end node)))
            (when (< start end)
              (let ((blank (replace-regexp-in-string
                            "[^\n]" " " (buffer-substring start end))))
                (goto-char start)
                (delete-region start end)
                (insert blank))))))
      (cons markdown-parser (treesit-parser-create 'markdown-inline nil t)))))

(defvar thb-md-render--inline-parser nil
  "Dynamic binding: the one markdown-inline parser for the current render.
Bound by `thb-md-render-file'.")

(defvar thb-md-render--inline-active nil
  "Non-nil while `thb-md-render--inline-walk' is using inline nodes.")

(defun thb-md-render--inline-nodes (start end)
  "Parse START..END alone and return its top-level inline nodes.
Each inline range is parsed on its own: one parse over every range would
read them as one continuous text, so a backtick or asterisk in one
paragraph could pair with one in the next and pull the source between them
into the output.  Re-pointing a single parser costs a reparse of one range,
far less than creating a parser per range.

Coupling: re-pointing outdates every node from the previous call, so the
caller must finish with these nodes before the next call.
`thb-md-render--inline-walk' enforces that it is never re-entered."
  ;; Ranges are validated against the current buffer, which during a render
  ;; is the output buffer; switch to the parser's own buffer.
  (with-current-buffer (treesit-parser-buffer thb-md-render--inline-parser)
    (treesit-parser-set-included-ranges thb-md-render--inline-parser
                                        (list (cons start end)))
    (treesit-node-children
     (treesit-parser-root-node thb-md-render--inline-parser))))

(defun thb-md-render--text-without-continuations (node)
  "Return NODE's source text without its `block_continuation' children.
Quote markers and list indentation on continuation lines belong to the
enclosing block, not to code content."
  (let ((pos (treesit-node-start node))
        (parts nil))
    (dolist (c (thb-md-render--children-of-type node "block_continuation"))
      (push (thb-md-render--src-text pos (treesit-node-start c)) parts)
      (setq pos (treesit-node-end c)))
    (push (thb-md-render--src-text pos (treesit-node-end node)) parts)
    (apply #'concat (nreverse parts))))

(defun thb-md-render--children-of-type (node type)
  "Return direct children of NODE whose `treesit-node-type' is TYPE (a string)."
  (seq-filter (lambda (c) (equal (treesit-node-type c) type))
              (treesit-node-children node)))

(defun thb-md-render--first-child-of-type (node type)
  "Return NODE's first direct child of TYPE, or nil.
Stop at the first match instead of allocating and filtering a complete
child list."
  (let ((index 0)
        (count (treesit-node-child-count node))
        child)
    (while (and (< index count) (not child))
      (let ((candidate (treesit-node-child node index)))
        (when (equal (treesit-node-type candidate) type)
          (setq child candidate)))
      (cl-incf index))
    child))

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

(defvar thb-md-render--blank-line-suppressed nil
  "Non-nil when the next block should start directly on the current line.
Bound by `thb-md-render--walk-blockquote' after an alert label;
`thb-md-render--ensure-blank-line' consumes it.")

(defun thb-md-render--ensure-blank-line ()
  "Ensure there is at least one blank line before point.
Do nothing at buffer start or after two newlines, append one newline after
one, and append two after other text.  Inspect only adjacent characters so
the work is independent of the output buffer's current position.  Once
`thb-md-render--blank-line-suppressed' is set, only clear it."
  (if thb-md-render--blank-line-suppressed
      (setq thb-md-render--blank-line-suppressed nil)
    (unless (= (point) (point-min))
      (cond
       ((and (eq (char-before) ?\n)
             (> (point) (1+ (point-min)))
             (eq (char-before (1- (point))) ?\n)))
       ((eq (char-before) ?\n)
        (insert "\n"))
       (t
        (insert "\n\n"))))))

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
Innermost face is on top.  `thb-md-render--current-face' combines the
whole stack so nested markup keeps its context's attributes.")

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

(defun thb-md-render--current-face ()
  "Return the face for inline text at the current nesting depth.
Combine every face on `thb-md-render--inline-face-stack', innermost first,
so nested markup keeps its context (a heading's height, an outer
emphasis's slant).  Top-level prose uses `thb-md-render-body'."
  (let ((stack thb-md-render--inline-face-stack))
    (cond ((null stack) 'thb-md-render-body)
          ((null (cdr stack)) (car stack))
          (t stack))))

(defun thb-md-render--face-with (face)
  "Return the current inline face with FACE nested innermost."
  (let ((thb-md-render--inline-face-stack
         (cons face thb-md-render--inline-face-stack)))
    (thb-md-render--current-face)))

(defun thb-md-render--join-soft-breaks (start)
  "Render the soft line breaks between START and point as single spaces.
Also drop whitespace at the start of an output line, which only follows a
hard line break; it is source indentation or a blanked block marker."
  (save-excursion
    (let ((end (point-marker)))
      (goto-char start)
      (when (bolp)
        (delete-region start (progn (skip-chars-forward " \t" end) (point))))
      (while (re-search-forward "[ \t]*\n[ \t]*" end t)
        (replace-match " " t t))
      (set-marker end nil))))

(defun thb-md-render--emit-plain (start end)
  "Emit plain source text in the range START..END without allocating it.
Soft line breaks become spaces.  Apply `thb-md-render--current-face'."
  (when (< start end)
    (let ((output-start (point)))
      (insert-buffer-substring-no-properties thb-md-render--src-buffer
                                             start end)
      (thb-md-render--join-soft-breaks output-start)
      (put-text-property output-start (point) 'face
                         (thb-md-render--current-face)))))

(defun thb-md-render--emit-nested (node face)
  "Emit NODE's inline content with FACE nested, keeping inner markup."
  (when node
    (let ((thb-md-render--inline-face-stack
           (cons face thb-md-render--inline-face-stack)))
      (thb-md-render--walk-inline-range (treesit-node-children node)
                                        (treesit-node-start node)
                                        (treesit-node-end node)))))

(defvar thb-md-render-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'thb-md-render-follow-link)
    (define-key map [mouse-2] #'thb-md-render-follow-link-at-mouse)
    ;; With `mouse-face' set, this makes mouse-1 follow the link too.
    (define-key map [follow-link] 'mouse-face)
    map)
  "Keymap active on rendered links.
A `keymap' text property takes precedence over evil's state maps, so
RET follows a link in motion state too.")

(defun thb-md-render--emit-link (text-node url)
  "Emit TEXT-NODE's content as a link to URL, keeping its inner markup.
A nil URL emits the text in link face without link behaviour."
  (let ((start (point)))
    (thb-md-render--emit-nested text-node 'thb-md-render-link-text)
    (when url
      (add-text-properties start (point)
                           (list 'help-echo url 'thb-md-link url
                                 'mouse-face 'highlight
                                 'keymap thb-md-render-link-map)))))

(defvar thb-md-render--link-definitions nil
  "Dynamic binding: hash table of normalized label -> URL for one render.
Bound by `thb-md-render-file' from the document's
`link_reference_definition' nodes.")

(defun thb-md-render--normalize-label (label)
  "Return reference LABEL as matched against definitions.
Strip surrounding brackets, collapse whitespace and fold case, as
CommonMark specifies."
  (downcase (string-trim (replace-regexp-in-string
                          "[ \t\n]+" " " (string-trim label "\\[" "\\]")))))

(defun thb-md-render--collect-link-definitions (root)
  "Return a hash table of the link reference definitions under ROOT.
The first definition of a label wins.  Destinations are decoded by
`thb-md-render--link-destination'."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (def (treesit-query-capture
                  root '((link_reference_definition) @d) nil nil t))
      (let ((label (thb-md-render--first-child-of-type def "link_label"))
            (dest (thb-md-render--first-child-of-type def "link_destination")))
        (when (and label dest)
          (let ((key (thb-md-render--normalize-label
                      (thb-md-render--node-text label))))
            (unless (or (string-empty-p key) (gethash key table))
              (puthash key (thb-md-render--link-destination
                            (thb-md-render--node-text dest))
                       table))))))
    table))

(defun thb-md-render--link-destination (text)
  "Return link destination source TEXT as the URL it denotes.
Strip the <...> form and decode backslash escapes and entity references,
as CommonMark does, so `[a](<my file.md>)' names `my file.md'."
  (let ((dest (if (string-match "\\`<\\(.*\\)>\\'" text) (match-string 1 text) text)))
    (replace-regexp-in-string
     "\\\\\\([[:punct:]]\\)\\|&#?[[:alnum:]]+;"
     (lambda (m)
       (if (string-prefix-p "\\" m)
           (substring m 1)
         (thb-md-render--decode-entity m)))
     dest t t)))

(defun thb-md-render--link-definition (label)
  "Return the URL defined for reference LABEL, or nil."
  (and label thb-md-render--link-definitions
       (gethash (thb-md-render--normalize-label label)
                thb-md-render--link-definitions)))

(defconst thb-md-render--named-entities
  '(("amp" . "&") ("lt" . "<") ("gt" . ">") ("quot" . "\"") ("apos" . "'")
    ("nbsp" . " ") ("copy" . "©") ("reg" . "®") ("trade" . "™")
    ("mdash" . "—") ("ndash" . "–") ("hellip" . "…") ("middot" . "·")
    ("lsquo" . "‘") ("rsquo" . "’") ("ldquo" . "“") ("rdquo" . "”")
    ("larr" . "←") ("rarr" . "→") ("times" . "×") ("deg" . "°"))
  "HTML named entities decoded in prose.  Unlisted names render verbatim.")

(defun thb-md-render--decode-entity (text)
  "Return the character for entity or numeric reference TEXT.
Invalid code points become U+FFFD; unknown names return TEXT unchanged."
  (let ((code (cond
               ((string-match "\\`&#[xX]\\([[:xdigit:]]+\\);\\'" text)
                (string-to-number (match-string 1 text) 16))
               ((string-match "\\`&#\\([0-9]+\\);\\'" text)
                (string-to-number (match-string 1 text))))))
    (cond
     (code
      (if (and (integerp code) (< 0 code #x110000)
               (not (<= #xD800 code #xDFFF)))
          (string code)
        "�"))
     ((string-match "\\`&\\([[:alnum:]]+\\);\\'" text)
      (or (cdr (assoc (match-string 1 text) thb-md-render--named-entities))
          text))
     (t text))))

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
Parse the range alone with `thb-md-render--inline-nodes', then emit its
nodes and the plain text between them.  Nothing reached from here renders
another inline range, so the nodes stay valid until this call returns."
  (when thb-md-render--inline-active
    (error "Nested inline walk would outdate the nodes in use"))
  (when (< start end)
    (let ((thb-md-render--inline-active t)
          (cursor start))
      (dolist (child (thb-md-render--inline-nodes start end))
        (thb-md-render--emit-plain cursor (treesit-node-start child))
        (thb-md-render--emit-inline-node child)
        (setq cursor (treesit-node-end child)))
      ;; Trailing plain text up to the range end.
      (thb-md-render--emit-plain cursor end))))

(defun thb-md-render--emit-inline-node (node)
  "Emit a single inline parser NODE (emphasis, code_span, link, etc.)."
  (let* ((type  (treesit-node-type node))
         (face  (thb-md-render--inline-face-for type)))
    (pcase type
      ;; Markdown escapes punctuation in prose, but not inside code spans.
      ("backslash_escape"
       (thb-md-render--emit-plain (1+ (treesit-node-start node))
                                  (treesit-node-end node)))
      ;; Backslash or two-space hard break: a real output line break.
      ("hard_line_break"
       (thb-md-render--emit "\n" (thb-md-render--current-face)))
      ((or "entity_reference" "numeric_character_reference")
       (thb-md-render--emit (thb-md-render--decode-entity
                             (thb-md-render--node-text node))
                            (thb-md-render--current-face)))
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
      ;; Line endings inside a code span render as spaces.
      ("code_span"
       (let* ((delims (thb-md-render--children-of-type node "code_span_delimiter"))
              (text (if (= (length delims) 2)
                        (thb-md-render--src-text
                         (treesit-node-end (car delims))
                         (treesit-node-start (cadr delims)))
                      ;; Malformed; fall back to whole-node text.
                      (thb-md-render--node-text node))))
         ;; Continuation indentation (or a blanked quote marker) belongs to
         ;; the line ending, which renders as one space.
         (thb-md-render--emit (replace-regexp-in-string
                               "[ \t]*\n[ \t]*" " " text t t)
                              (thb-md-render--face-with
                               'thb-md-render-code-inline))))
      ;; Inline link [text](url).  The grammar excludes brackets from
      ;; link_text and nests the text's own markup (emphasis, code, escapes)
      ;; under it.
      ("inline_link"
       (let ((dest-node (thb-md-render--first-child-of-type node "link_destination")))
         (thb-md-render--emit-link
          (thb-md-render--first-child-of-type node "link_text")
          (and dest-node (thb-md-render--link-destination
                          (thb-md-render--node-text dest-node))))))
      ;; Reference links [text][label], [label][] and [label].  Without a
      ;; matching definition they are literal text, as in CommonMark.
      ((or "full_reference_link" "collapsed_reference_link" "shortcut_link")
       (let* ((text-node (thb-md-render--first-child-of-type node "link_text"))
              (label-node (thb-md-render--first-child-of-type node "link_label"))
              (url (thb-md-render--link-definition
                    (if label-node
                        (thb-md-render--node-text label-node)
                      (and text-node (thb-md-render--node-text text-node))))))
         (if url
             (thb-md-render--emit-link text-node url)
           (let ((face (thb-md-render--current-face)))
             (thb-md-render--emit "[" face)
             (when text-node
               (thb-md-render--walk-inline-range (treesit-node-children text-node)
                                                 (treesit-node-start text-node)
                                                 (treesit-node-end text-node)))
             (thb-md-render--emit "]" face)
             (pcase type
               ("full_reference_link"
                (thb-md-render--emit (thb-md-render--node-text label-node) face))
               ("collapsed_reference_link"
                (thb-md-render--emit "[]" face)))))))
      ;; Image ![alt](src): show alt + faint URL.  No inline image yet.
      ("image"
       (let* ((alt-node (thb-md-render--first-child-of-type node "image_description"))
              (dest-node (thb-md-render--first-child-of-type node "link_destination"))
              (src (and dest-node (thb-md-render--node-text dest-node)))
              (start (point)))
         (thb-md-render--emit "🖼 " (thb-md-render--face-with
                                    'thb-md-render-list-marker))
         (thb-md-render--emit-nested alt-node 'thb-md-render-link-text)
         (when src
           (thb-md-render--emit " " (thb-md-render--current-face))
           (thb-md-render--emit src (thb-md-render--face-with
                                     'thb-md-render-link-url)))
         ;; Images contribute no text to a heading's title or anchor.
         (put-text-property start (point) 'thb-md-render--image t)))
      ;; Fallthrough: just emit the source text with current face.
      (_
       (let ((start (treesit-node-start node))
             (end   (treesit-node-end   node)))
         (thb-md-render--emit-plain start end))))))

;;;; Block rendering -----------------------------------------------------

(defvar thb-md-render--list-state nil
  "Stack of (KIND COUNTER SUFFIX) entries while inside lists.
KIND is `unordered' or `ordered'.  COUNTER is the next ordered-list
ordinal, and SUFFIX preserves the source marker style, either `.' or `)'.")

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
  "Return the configured face specification for heading LEVEL."
  (let ((scale (or (nth (1- level) thb-md-render-heading-scales) 1.0)))
    (list :inherit (intern (format "thb-md-render-h%d" level))
          :height scale)))

(defvar thb-md-render--anchor-counts nil
  "Dynamic binding: hash table of anchor -> times seen, for one render.")

(defun thb-md-render--anchor (title)
  "Return the GitHub-style anchor for heading TITLE, unique in this render.
Downcase, drop everything but letters, digits, spaces, `-' and `_', turn
each space into `-', and suffix repeats with -1, -2, ..."
  (let* ((base (string-replace
                " " "-" (replace-regexp-in-string
                         "[^[:alnum:] _-]" "" (downcase title))))
         (seen (gethash base thb-md-render--anchor-counts 0)))
    (puthash base (1+ seen) thb-md-render--anchor-counts)
    (if (zerop seen) base (format "%s-%d" base seen))))

(defun thb-md-render--emit-heading (level inline &optional atx)
  "Emit heading LEVEL with INLINE's content and record it for navigation.
ATX non-nil drops an ATX heading's closing #s; a setext heading keeps them.
The `thb-md-render--heading' property holds one (LEVEL ANCHOR TITLE) list
over the heading text; prose wrapping can split it across lines, so
readers treat `eq' values as one heading."
  (thb-md-render--ensure-blank-line)
  (let ((start (point)))
    (let ((thb-md-render--inline-face-stack
           (list (thb-md-render--heading-face level))))
      (when inline
        (thb-md-render--inline-walk
         (treesit-node-start inline)
         (if atx
             (thb-md-render--heading-content-end inline)
           (treesit-node-end inline)))))
    (when (< start (point))
      ;; The title is the heading's text without image placeholders.
      (let ((title (string-trim
                    (mapconcat (lambda (pos)
                                 (if (get-text-property pos 'thb-md-render--image)
                                     ""
                                   (string (char-after pos))))
                               (number-sequence start (1- (point))) ""))))
        (put-text-property start (point) 'thb-md-render--heading
                           (list level (thb-md-render--anchor title) title)))))
  (thb-md-render--newline 2))

(defun thb-md-render--heading-content-end (inline)
  "Return where heading INLINE's content ends, before any closing #s.
CommonMark drops an ATX heading's optional closing sequence: `#'s preceded
by a space (or making up the whole content) and followed only by spaces.
An escaped `\\#' is content."
  (let* ((start (treesit-node-start inline))
         (end (treesit-node-end inline))
         (text (thb-md-render--src-text start end)))
    (cond ((string-match "\\`#+[ \t]*\\'" text) start)
          ((string-match "[ \t]+#+[ \t]*\\'" text) (+ start (match-beginning 0)))
          (t end))))

(defun thb-md-render--walk-heading (node)
  (thb-md-render--emit-heading
   (thb-md-render--atx-level node)
   (thb-md-render--first-child-of-type node "inline")
   t))

(defun thb-md-render--walk-setext-heading (node)
  (let ((content (thb-md-render--first-child-of-type node "paragraph")))
    (thb-md-render--emit-heading
     (if (thb-md-render--first-child-of-type node "setext_h1_underline") 1 2)
     (and content (thb-md-render--first-child-of-type content "inline")))))

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
Create and prime one on first use; subsequent calls reuse it.  If MODE
initialization fails, kill the partially initialized buffer before the
error is propagated to the caller's plain-text fallback."
  (let ((cached (gethash mode thb-md-render--fontify-buffers)))
    (if (and cached (buffer-live-p cached))
        cached
      (when cached
        (remhash mode thb-md-render--fontify-buffers))
      (let ((buf (generate-new-buffer
                  (format " *thb-md-fontify-cache: %s*" mode) t))
            initialized)
        (unwind-protect
            (progn
              (with-current-buffer buf
                (delay-mode-hooks (funcall mode)))
              (puthash mode buf thb-md-render--fontify-buffers)
              (setq initialized t)
              buf)
          (unless initialized
            (when (buffer-live-p buf)
              (kill-buffer buf))))))))

(defun thb-md-render-fontify-cleanup ()
  "Kill all cached fontification buffers.  Hooked to `kill-emacs-hook'."
  (maphash (lambda (_mode buf)
             (when (buffer-live-p buf) (kill-buffer buf)))
           thb-md-render--fontify-buffers)
  (clrhash thb-md-render--fontify-buffers))
(add-hook 'kill-emacs-hook #'thb-md-render-fontify-cleanup)

(defun thb-md-render--fontify-face-only-string ()
  "Return the current buffer text carrying only its `face' properties."
  (let ((result (buffer-substring-no-properties (point-min) (point-max)))
        (position (point-min))
        (origin (point-min))
        next face)
    (while (< position (point-max))
      (setq next (next-single-property-change position 'face nil (point-max))
            face (get-text-property position 'face))
      (when face
        (put-text-property (- position origin) (- next origin)
                           'face face result))
      (setq position next))
    result))

(defun thb-md-render--fontify-code (text lang)
  "Return TEXT fontified according to LANG's configured major mode.
Language lookup is case-insensitive.  Return TEXT unchanged on any failure,
including an unknown language, missing mode, or mode initialization error.

Reuse a cached buffer per mode, but explicitly flush and ensure font-lock
for every block.  The returned string carries only display-relevant `face'
properties; syntax, composition, and fontification bookkeeping remain in
the scratch buffer."
  (let* ((language (and (stringp lang) (downcase (string-trim lang))))
         (mode (cdr (assoc language thb-md-render-language-mode-alist))))
    (if (and mode (fboundp mode))
        (condition-case _err
            (with-current-buffer
                (thb-md-render--fontify-buffer-for-mode mode)
              (let ((inhibit-read-only t))
                (widen)
                (erase-buffer)
                (insert text)
                (setq font-lock-fontified nil)
                (when (fboundp 'syntax-ppss-flush-cache)
                  (syntax-ppss-flush-cache (point-min)))
                (font-lock-flush (point-min) (point-max))
                (font-lock-ensure (point-min) (point-max))
                (thb-md-render--fontify-face-only-string)))
          (error text))
      text)))

(defun thb-md-render--walk-fenced-code (node)
  "Render a fenced code block as a unified card:
  - optional top blank line (with bg) for vertical padding
  - optional language tag line (small label, in code-fence-info face)
  - the code itself, each line prefixed with two spaces for left padding
    and fontified by its configured language mode when available
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
        (let* ((raw (thb-md-render--text-without-continuations content))
               (trimmed (string-trim-right raw "\n"))
               (fontified (if (and lang (> (length trimmed) 0))
                              (thb-md-render--fontify-code trimmed lang)
                            trimmed)))
          ;; Insert the (potentially fontified) content one line at a time
          ;; so each line gets the left-padding indent.  Text properties on
          ;; `fontified' are preserved by `insert' so per-token faces
          ;; survive the line splitting.
          (let ((lines (split-string fontified "\n")))
            (dolist (line lines)
              (insert indent line "\n")))))
      ;; Bottom padding line.
      (insert "\n")
      ;; Apply the code-block face exactly once over the complete card.
      ;; Append it after token faces so syntax foregrounds win while the
      ;; code-block face supplies fixed pitch and the shared background.
      (font-lock-append-text-property block-start (point)
                                      'face 'thb-md-render-code-block)
      ;; Code blocks are never prose-wrapped: each line extends right.
      (thb-md-render--mark-nowrap block-start (point))))
  (thb-md-render--newline 1))

(defun thb-md-render--walk-indented-code (node)
  (thb-md-render--ensure-blank-line)
  (let* ((raw (thb-md-render--text-without-continuations node))
         ;; The node starts at the first line's 4-space code indent.  On later
         ;; lines `block_continuation' covers the container prefix AND the
         ;; code indent, so excluding it already removed theirs; trimming
         ;; those lines again would drop relative indentation.
         (cleaned (if (string-prefix-p "    " raw) (substring raw 4) raw))
         (trimmed (string-trim-right cleaned "\n+"))
         (code-start (point)))
    (thb-md-render--emit (concat trimmed "\n") 'thb-md-render-code-block)
    ;; Indented code is never prose-wrapped: each line extends right.
    (thb-md-render--mark-nowrap code-start (point)))
  (thb-md-render--newline 1))

;;;; Block: list ---------------------------------------------------------

(defun thb-md-render--ordered-marker (list-node)
  "Return LIST-NODE's first ordered marker node, or nil."
  (when-let* ((item (thb-md-render--first-child-of-type list-node "list_item")))
    (or (thb-md-render--first-child-of-type item "list_marker_dot")
        (thb-md-render--first-child-of-type item "list_marker_parenthesis"))))

(defun thb-md-render--list-kind (list-node)
  "Return `ordered' or `unordered' for LIST-NODE."
  (if (thb-md-render--ordered-marker list-node) 'ordered 'unordered))

(defun thb-md-render--list-entry (list-node)
  "Return initial (KIND COUNTER SUFFIX) state for LIST-NODE."
  (if-let* ((marker (thb-md-render--ordered-marker list-node)))
      (let ((text (thb-md-render--node-text marker)))
        (if (string-match "\\`[[:space:]]*\\([0-9]+\\)\\([.)]\\)" text)
            (list 'ordered
                  (string-to-number (match-string 1 text))
                  (match-string 2 text))
          ;; Fresh lists: `thb-md-render--walk-list-item' mutates the counter.
          (list 'ordered 1 ".")))
    (list 'unordered nil nil)))

(defun thb-md-render--task-prefix (task)
  "Return a configured or safe fallback prefix for TASK marker node."
  (let* ((kind (pcase (treesit-node-type task)
                 ("task_list_marker_checked" "checked")
                 (_ "unchecked")))
         (configured (alist-get kind thb-md-render-task-glyphs
                                nil nil #'equal)))
    (or configured
        (if (equal kind "checked") "[x] " "[ ] "))))

(defun thb-md-render--walk-list (node)
  (let ((top-level (null thb-md-render--list-state)))
    (when top-level
      (thb-md-render--ensure-blank-line))
    (let ((thb-md-render--list-state
           (cons (thb-md-render--list-entry node)
                 thb-md-render--list-state)))
      (dolist (item (thb-md-render--children-of-type node "list_item"))
        (thb-md-render--walk-list-item item)))
    ;; Each item already ends in one newline.  Only a top-level list needs
    ;; the additional separator; adding it to nested lists creates gaps.
    (when top-level
      (thb-md-render--newline 1))))

(defun thb-md-render--walk-list-item (node)
  (let* ((entry (car thb-md-render--list-state))
         (kind (nth 0 entry))
         (ordinal (nth 1 entry))
         (suffix (nth 2 entry))
         (task (or (thb-md-render--first-child-of-type node "task_list_marker_unchecked")
                   (thb-md-render--first-child-of-type node "task_list_marker_checked")))
         (depth (1- (length thb-md-render--list-state)))
         (indent (make-string (* depth 2) ?\s))
         (prefix
          (cond
           (task (thb-md-render--task-prefix task))
           ((eq kind 'ordered) (format "%d%s " ordinal suffix))
           (t thb-md-render-bullet))))
    ;; Task-list items still consume an ordinal when their enclosing list is
    ;; ordered, even though the checkbox replaces the visible number.
    (when (eq kind 'ordered)
      (setf (nth 1 entry) (1+ ordinal)))
    (insert indent)
    (thb-md-render--emit prefix 'thb-md-render-list-marker)
    ;; Total left padding for wrapped paragraph lines = item indent + prefix.
    ;; Content after the first line (hard breaks, later paragraphs, code,
    ;; quotes, tables) hangs at the same column.
    (let ((wrap-pad (concat indent (make-string (length prefix) ?\s)))
          (content-emitted nil))
      (dolist (c (treesit-node-children node))
        (let ((type (treesit-node-type c)))
          (pcase type
            ("paragraph"
             (when content-emitted
               ;; A later paragraph of a loose item: one blank line (blocks
               ;; such as quotes and code cards already end on one), then
               ;; the indent.
               (thb-md-render--ensure-blank-line)
               (insert wrap-pad))
             (let ((paragraph-start (point))
                   (inline (thb-md-render--first-child-of-type c "inline")))
               (when inline
                 (thb-md-render--inline-walk
                  (treesit-node-start inline) (treesit-node-end inline)))
               (thb-md-render--newline 1)
               (thb-md-render--indent-lines
                (save-excursion (goto-char paragraph-start)
                                (line-beginning-position 2))
                (point) wrap-pad)
               (put-text-property paragraph-start (point)
                                  'wrap-prefix wrap-pad))
             (setq content-emitted t))
            ("list"
             ;; A marker-only parent has not emitted a paragraph newline yet;
             ;; keep its marker and the first nested marker on separate lines.
             (unless (bolp)
               (thb-md-render--newline 1))
             ;; Nested items set their own indentation and wrap prefix.  Do
             ;; not add separators or overwrite those properties here.
             (thb-md-render--walk-list c)
             (setq content-emitted t))
            ;; Skip marker children (already handled).
            ((or "list_marker_minus" "list_marker_plus"
                 "list_marker_star" "list_marker_dot"
                 "list_marker_parenthesis"
                 "task_list_marker_unchecked" "task_list_marker_checked"
                 "block_continuation") nil)
            (_
             (let ((block-start (point)))
               (thb-md-render--walk c)
               (thb-md-render--indent-lines block-start (point) wrap-pad)
               ;; Keep a nested quote's own hang (its bar) under the item.
               (thb-md-render--compose-wrap-prefix block-start (point)
                                                   wrap-pad t))
             (setq content-emitted t))))))))

(defun thb-md-render--indent-lines (start end prefix)
  "Insert PREFIX at the start of each line that begins in START..END.
Skip empty lines, except a code card's padding lines (tagged
`thb-md-render--nowrap'), whose background must start at the indent."
  (save-excursion
    (let ((end-marker (copy-marker end)))
      (goto-char start)
      (unless (bolp) (forward-line 1))
      (while (< (point) end-marker)
        (when (or (not (eolp))
                  (get-text-property (point) 'thb-md-render--nowrap))
          (insert prefix))
        (forward-line 1))
      (set-marker end-marker nil))))

(defun thb-md-render--compose-wrap-prefix (start end prefix &optional existing-only face)
  "Prepend PREFIX to the `wrap-prefix' of the text in START..END.
Where text has no `wrap-prefix', set it to PREFIX, unless EXISTING-ONLY.
FACE, when non-nil, is appended over the whole resulting prefix, so the
hang that wrapping inserts carries the container's background (a quote's
tint) under a nested list's indent too."
  (let ((pos start))
    (while (< pos end)
      (let ((next (next-single-property-change pos 'wrap-prefix nil end))
            (old (get-text-property pos 'wrap-prefix)))
        (when (or old (not existing-only))
          (let ((new (concat prefix old)))
            (when face
              (font-lock-append-text-property 0 (length new) 'face face new))
            (put-text-property pos next 'wrap-prefix new)))
        (setq pos next)))))

;;;; Block: blockquote ---------------------------------------------------

(defconst thb-md-render--alert-types
  '("NOTE" "TIP" "IMPORTANT" "WARNING" "CAUTION")
  "GitHub alert types, as written inside `> [!TYPE]'.")

(defun thb-md-render--alert (node)
  "Return (TYPE . BODY-START) if block quote NODE is a GitHub alert, else nil.
An alert's first block is a paragraph whose first line is exactly
`[!TYPE]', with TYPE in `thb-md-render--alert-types' in any case.  TYPE
is returned downcased; BODY-START is where the paragraph's text resumes."
  (when-let* ((first (seq-find (lambda (c)
                                 (not (member (treesit-node-type c)
                                              '("block_quote_marker"
                                                "block_continuation"))))
                               (treesit-node-children node)))
              ((equal (treesit-node-type first) "paragraph"))
              (inline (thb-md-render--first-child-of-type first "inline"))
              (start (treesit-node-start inline))
              (end (treesit-node-end inline))
              (text (thb-md-render--src-text start end))
              ((let ((case-fold-search t))
                 (string-match (concat "\\`\\[!\\("
                                       (regexp-opt thb-md-render--alert-types)
                                       "\\)\\][ \t]*\\(?:\n\\|\\'\\)")
                               text))))
    (let ((type (downcase (match-string 1 text)))
          (body (+ start (match-end 0))))
      ;; Skip the blanked `>' marker and indentation of the next line.
      (with-current-buffer thb-md-render--src-buffer
        (save-excursion
          (goto-char body)
          (skip-chars-forward " \t\n" end)
          (cons type (point)))))))

(defvar thb-md-render--quote-depth 0
  "Number of block quotes enclosing the block being rendered.")

(defun thb-md-render--walk-blockquote (node)
  (thb-md-render--ensure-blank-line)
  (let* ((quote-start (point))
         (alert (thb-md-render--alert node))
         (bar-face (if alert
                       (intern (format "thb-md-render-alert-%s" (car alert)))
                     'thb-md-render-blockquote-marker))
         (alert-paragraph-done nil))
    (when alert
      (thb-md-render--emit (capitalize (car alert)) bar-face)
      (thb-md-render--newline 1))
    ;; Walk children of the blockquote, skipping source-only markers.
    (let ((thb-md-render--quote-depth (1+ thb-md-render--quote-depth))
          ;; The alert's body starts directly under its label, whatever block
          ;; comes first (paragraph text, a code card, a list).
          (thb-md-render--blank-line-suppressed (and alert t)))
      (dolist (c (treesit-node-children node))
        (let ((type (treesit-node-type c)))
          (pcase type
            ((or "block_quote_marker" "block_continuation") nil)
            ((and "paragraph" (guard (and alert (not alert-paragraph-done))))
             ;; The alert's first paragraph: render only the text after the
             ;; `[!TYPE]' line, directly under the label.
             (setq alert-paragraph-done t)
             (let ((inline (thb-md-render--first-child-of-type c "inline")))
               (when (< (cdr alert) (treesit-node-end inline))
                 (setq thb-md-render--blank-line-suppressed nil)
                 (thb-md-render--inline-walk (cdr alert) (treesit-node-end inline))
                 (thb-md-render--newline 2))))
            (_ (thb-md-render--walk c))))))
    ;; Face the content and its terminating newline, but not the trailing
    ;; separator.  Prefix every logical quote line with the configured marker;
    ;; wrapping later preserves that marker as part of the first-line indent.
    (let* ((content-end
            (save-excursion
              (skip-chars-backward "\n")
              ;; A code card's empty bottom padding line is content: its
              ;; newline and the one before it are both tagged nowrap.
              (while (and (eq (char-after) ?\n)
                          (eq (char-after (1+ (point))) ?\n)
                          (get-text-property (point) 'thb-md-render--nowrap)
                          (get-text-property (1+ (point)) 'thb-md-render--nowrap))
                (forward-char 1))
              (point)))
           (quote-end (min (point) (1+ content-end))))
      (when (> content-end quote-start)
        ;; Pad the outermost quote with a tinted line above and below, like
        ;; a code card, unless a code card at that edge already pads it.
        (when (zerop thb-md-render--quote-depth)
          (cl-flet ((card-pad-p (pos) (and (eq (char-after pos) ?\n)
                                           (get-text-property pos 'thb-md-render--nowrap))))
            (unless (and (card-pad-p content-end)
                         (eq (char-before content-end) ?\n))
              ;; QUOTE-END can equal point (an alert with no body); a marker
              ;; that advances keeps later output after the padding.
              (let ((after (copy-marker (point) t)))
                (goto-char quote-end)
                (insert "\n")
                (goto-char after)
                (set-marker after nil))
              (cl-incf quote-end))
            (unless (card-pad-p quote-start)
              (save-excursion (goto-char quote-start) (insert "\n"))
              (cl-incf quote-end))))
        (let ((end-marker (copy-marker quote-end t)))
          (save-excursion
            (goto-char quote-start)
            (while (< (point) end-marker)
              (let ((marker-start (point)))
                (insert thb-md-render-blockquote-prefix)
                (put-text-property marker-start (point) 'face bar-face))
              (forward-line 1)))
          ;; Append the quote face so the marker face remains the leading
          ;; face while the shared background still extends across each row.
          (font-lock-append-text-property quote-start end-marker
                                          'face 'thb-md-render-blockquote)
          ;; Prepend rather than replace, so nested quotes and lists keep
          ;; their own hang after this quote's bar.
          (thb-md-render--compose-wrap-prefix
           quote-start end-marker
           (propertize thb-md-render-blockquote-prefix 'face bar-face)
           nil 'thb-md-render-blockquote)
          (set-marker end-marker nil))))))

;;;; Block: pipe table -------------------------------------------------

;; GFM tables.  Tree-sitter-markdown emits:
;;
;;   (pipe_table
;;     (pipe_table_header (pipe_table_cell)+)
;;     (pipe_table_delimiter_row (pipe_table_delimiter_cell)+)
;;     (pipe_table_row (pipe_table_cell)+)+)
;;
;; Each cell is a leaf at the markdown-grammar level; its inline content
;; (code spans, emphasis, etc.) is parsed by markdown-inline: each cell is
;; one inline range for `thb-md-render--inline-nodes', so cells get
;; tokenized like paragraph content.
;;
;; Render strategy: tables go in `thb-md-render-table' (fixed-pitch) so
;; columns align visually with monospace.  Each cell's inline content is
;; rendered to a propertized string first (pass 1), so column widths are
;; measured from the *rendered* display width -- not the source markdown
;; (which over-counts `[text](url)' links and `code' backticks).  Layout
;; depends on the window, so it happens at wrap time
;; (`thb-md-render--layout-table'): a table that fits keeps its natural
;; column widths, two-space gaps and no vertical separators; a wider one
;; narrows its widest columns and wraps cells inside a `│' grid with rules
;; between rows, rather than folding into a multi-line mess.  Header row is
;; bold; a horizontal rule sits below the header.
;;
;; GFM cell alignment is encoded in the delimiter row's cell text:
;;   `:---'   left   (default)
;;   `---:'   right
;;   `:---:'  center
;;   `---'    default (left)
;; We parse those into a vector of alignment symbols and pad cells
;; accordingly when emitting.

(defvar thb-md-render--table-cell-buffer nil
  "Dynamic binding: reusable scratch buffer for table-cell rendering.")

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
Reuse the renderer-owned table scratch buffer; the cell's inline content
is parsed on its own like any other inline range."
  (unless (buffer-live-p thb-md-render--table-cell-buffer)
    (error "Table-cell scratch buffer is not initialized"))
  (with-current-buffer thb-md-render--table-cell-buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (thb-md-render--inline-walk (treesit-node-start cell)
                                  (treesit-node-end cell))
      (string-trim (buffer-string)))))

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

(defconst thb-md-render--table-min-column-width 8
  "Narrowest a column is squeezed to before its cells wrap.
A table that is still too wide at this width extends off to the right.")

(defun thb-md-render--table-fit-widths (natural target)
  "Return NATURAL column widths narrowed to sum to TARGET where possible.
Repeatedly narrow the column furthest above its floor, the smaller of its
natural width and `thb-md-render--table-min-column-width'."
  (let* ((widths (copy-sequence natural))
         (floors (cl-map 'vector
                         (lambda (w) (min w thb-md-render--table-min-column-width))
                         natural))
         (done nil))
    (while (and (not done) (> (cl-loop for w across widths sum w) target))
      (let ((best nil) (slack 0))
        (dotimes (i (length widths))
          (let ((s (- (aref widths i) (aref floors i))))
            (when (> s slack) (setq best i slack s))))
        (if best
            (aset widths best (1- (aref widths best)))
          (setq done t))))
    widths))

(defun thb-md-render--table-wrap-cell (s width)
  "Return propertized string S wrapped into lines of at most WIDTH columns.
Break at spaces; split a word wider than WIDTH by display width."
  (if (<= (string-width s) width)
      (list s)
    (let (lines line)
      (dolist (word (thb-md-render--split-words s))
        (while (> (string-width word) width)
          ;; Split the longest prefix that fits, after flushing the line.
          (when line (push line lines) (setq line nil))
          (let ((i 0) (w 0))
            (while (<= (+ w (char-width (aref word i))) width)
              (setq w (+ w (char-width (aref word i))) i (1+ i)))
            (push (substring word 0 (max i 1)) lines)
            (setq word (substring word (max i 1)))))
        (cond
         ((string-empty-p word))
         ((null line) (setq line word))
         ((<= (+ (string-width line) 1 (string-width word)) width)
          (setq line (concat line (thb-md-render--sep-space word) word)))
         (t (push line lines) (setq line word))))
      (when line (push line lines))
      (nreverse lines))))

(defun thb-md-render--table-emit-row (cells face widths aligns sep lead)
  "Emit one table row from CELLS (a vector of propertized strings).
Each cell is wrapped to its column width; the row takes as many lines as
its tallest cell.  Every line starts with LEAD (the container's quote bar
or list indent) and separates columns with SEP.  FACE, when non-nil, is
prepended over the row (e.g. the header's bold)."
  (let* ((n-cols (length widths))
         (lines (cl-loop for i below n-cols
                         collect (thb-md-render--table-wrap-cell
                                  (or (aref cells i) "") (aref widths i))))
         (height (apply #'max 1 (mapcar #'length lines))))
    (dotimes (k height)
      (insert lead)
      (let ((row-start (point)))
        (dotimes (i n-cols)
          (let ((last (= i (1- n-cols))))
            (thb-md-render--table-insert-aligned
             (or (nth k (nth i lines)) "") (aref widths i) (aref aligns i) last)
            (unless last (insert sep))))
        (insert "\n")
        ;; Apply table-content face over the whole row (fixed-pitch); PREPEND
        ;; so the table's fixed-pitch family wins over each cell's body face
        ;; (which inherits variable-pitch).  Cell foregrounds (code-inline,
        ;; link, etc.) still come through because thb-md-render-table sets no
        ;; foreground -- attribute lookup falls through to the per-cell face.
        (font-lock-prepend-text-property row-start (point)
                                         'face 'thb-md-render-table)
        (when face
          (font-lock-prepend-text-property row-start (point) 'face face))))))

(defun thb-md-render--table-rule (lead widths joint)
  "Insert LEAD and a rule across WIDTHS, crossing columns with JOINT."
  (insert lead)
  (let ((start (point)))
    (insert (mapconcat (lambda (w) (make-string w ?─)) widths joint) "\n")
    (put-text-property start (point) 'face 'thb-md-render-table-rule)))

(defun thb-md-render--layout-table (bol eol budget)
  "Replace the table placeholder line BOL..EOL with the table laid out.
Fit the table into BUDGET (see `thb-md-render--prose-budget-px') less the
line's LEAD, the text before the placeholder.  A table at natural width
keeps two-space gaps and one line per row; a narrower one squeezes its
widest columns, wraps cells, and separates columns and rows with rules so
multi-line rows stay readable.  Return the position after the table."
  (pcase-let* ((at (thb-md-render--property-start bol eol 'thb-md-render--table))
               (`(,rendered ,aligns ,n-cols)
                (get-text-property at 'thb-md-render--table))
               (lead (buffer-substring bol at))
               ;; A container (a quote) appended its face to the placeholder;
               ;; the rows replacing it need that face too.
               (container-face (get-text-property at 'face))
               (cell-px (max 1 (thb-md-render--string-pixel-width
                                (propertize "0" 'face 'thb-md-render-table))))
               (avail (floor (- budget (thb-md-render--string-pixel-width lead))
                             cell-px))
               (natural (thb-md-render--table-natural-widths rendered n-cols))
               (fits (<= (+ (cl-loop for w across natural sum w) (* 2 (1- n-cols)))
                         avail))
               (sep (if fits "  "
                      (propertize " │ " 'face 'thb-md-render-table-rule)))
               (widths (if fits natural
                         (thb-md-render--table-fit-widths
                          natural (- avail (* 3 (1- n-cols))))))
               (start bol))
    (delete-region bol (min (1+ eol) (point-max)))
    (goto-char bol)
    (let ((body-rows 0))
      (dolist (row rendered)
        (unless (cdr row)
          ;; Between wrapped body rows, a rule marks where each row ends.
          (when (and (not fits) (> body-rows 0))
            (thb-md-render--table-rule lead widths "─┼─"))
          (cl-incf body-rows))
        (thb-md-render--table-emit-row (car row) (cdr row) widths aligns sep lead)
        (when (eq (cdr row) 'thb-md-render-table-header)
          (thb-md-render--table-rule lead widths (if fits "──" "─┼─")))))
    (when container-face
      (font-lock-append-text-property start (point) 'face container-face))
    ;; Each laid-out row is one logical line that prose wrapping skips.
    (thb-md-render--mark-nowrap start (point))
    (point)))

(defun thb-md-render--walk-pipe-table (node)
  "Render table NODE's cells now; lay the table out at wrap time.
Column widths depend on the window, so emit one placeholder that
`thb-md-render--rewrap-prose' replaces via `thb-md-render--layout-table'
on every render and reflow."
  (thb-md-render--ensure-blank-line)
  (let* ((rows   (thb-md-render--table-rows node))
         (n-cols (if rows (length (car (car rows))) 0)))
    (when (> n-cols 0)
      (let ((aligns (thb-md-render--table-alignments node n-cols))
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
            (start (point)))
        (insert (propertize " " 'thb-md-render--table
                            (list rendered aligns n-cols))
                "\n")
        (thb-md-render--mark-nowrap start (point)))))
  (thb-md-render--newline 1))

;;;; Block: thematic break ----------------------------------------------

(defun thb-md-render--walk-thematic-break ()
  "Emit a one-glyph rule placeholder tagged `thb-md-render--rule'.
Its width depends on the window, so `thb-md-render--rewrap-prose' redraws
it to the wrap budget on every render and reflow."
  (thb-md-render--ensure-blank-line)
  (let ((start (point)))
    (insert (propertize "─" 'thb-md-render--rule t) "\n\n")
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

(defvar thb-md-render--measure-frame nil
  "Frame for measurements during one wrapping pass, or nil to look it up.
Bound by `thb-md-render--rewrap-prose' so per-word measurement does not
resolve the wrapping window again.")

(defvar thb-md-render--space-widths nil
  "Separator-space widths keyed by face during one wrapping pass, or nil.")

(defun thb-md-render--string-pixel-width (string)
  "Measure STRING in the wrapping frame's units: pixels or terminal columns.
Graphical measurements include the render buffer's face remapping, so
`text-scale-set' and the window budget agree.  Terminal faces do not change
cell widths; use display columns, including double-width characters."
  (let ((frame (or thb-md-render--measure-frame
                   (let ((win (thb-md-render--wrapping-window)))
                     (if win (window-frame win) (selected-frame))))))
    (cond
     ((not (display-graphic-p frame))
      (string-width string))
     ((eq frame (selected-frame))
      (string-pixel-width string (current-buffer)))
     (t
      (with-selected-frame frame
        (string-pixel-width string (current-buffer)))))))

(defun thb-md-render--space-width (space)
  "Return the width of separator SPACE, cached by face within one pass."
  (if (not thb-md-render--space-widths)
      (thb-md-render--string-pixel-width space)
    (let ((face (get-text-property 0 'face space)))
      (or (gethash face thb-md-render--space-widths)
          (puthash face (thb-md-render--string-pixel-width space)
                   thb-md-render--space-widths)))))

(defvar-local thb-md-render--wrap-window nil
  "Window that owns hard wrapping for this preview buffer.
A buffer can be displayed in differently sized windows, but its inserted
newlines are shared.  Keeping one live owner stable prevents resize events
from making those windows alternately rewrap the same buffer.")

(defun thb-md-render--wrapping-window ()
  "Return this preview's stable live wrapping window, or nil.
Prefer the selected window when choosing an owner for the first time, then
retain that owner until it stops displaying the current buffer."
  (let ((buf (current-buffer)))
    (cond
     ((and (window-live-p thb-md-render--wrap-window)
           (eq (window-buffer thb-md-render--wrap-window) buf))
      thb-md-render--wrap-window)
     ((and (window-live-p (selected-window))
           (eq (window-buffer (selected-window)) buf))
      (setq thb-md-render--wrap-window (selected-window)))
     ((when-let* ((win (get-buffer-window buf t)))
        (setq thb-md-render--wrap-window win))))))

(defun thb-md-render--window-width-px ()
  "Return the wrapping window's text-area width in pixels, or nil."
  (when-let* ((win (thb-md-render--wrapping-window)))
    (window-body-width win t)))

(defun thb-md-render--prose-budget-px ()
  "Return the render-buffer-aware pixel budget for prose wrapping.
Use the stable wrapping window's live text area when displayed.  Otherwise,
measure the configured fallback column count with the current buffer's face
remapping; the first display is corrected by `thb-md-render--maybe-reflow'."
  (if-let* ((width (thb-md-render--window-width-px)))
      ;; Leave a scaled-character safety margin so a glyph landing exactly
      ;; on the right edge never trips the truncation glyph.
      (max (if (display-graphic-p
                (window-frame (thb-md-render--wrapping-window)))
               200 1)
           (- width (thb-md-render--string-pixel-width "M")))
    (thb-md-render--string-pixel-width
     (make-string (if (and (numberp thb-md-render-body-width)
                           (> thb-md-render-body-width 0))
                      thb-md-render-body-width 80)
                  ?M))))

(defvar-local thb-md-render--wrap-width nil
  "Owner-window text-area pixel width used for the last prose rewrap.")

(defvar-local thb-md-render--unwrapped-content nil
  "Propertized emitted document before width-dependent prose wrapping.
Resize reflow restores this string instead of rereading and reparsing source.")

(defvar-local thb-md-render--reflow-timer nil
  "Debounce timer for `thb-md-render--maybe-reflow'.")

(defvar-local thb-md-render--reflowing nil
  "Non-nil while a width-only reflow is updating margins and content.
This suppresses window-configuration feedback from margin resets.")

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

(defun thb-md-render--faced-newline (line)
  "Return a newline carrying LINE's last-char `face'.
A face with `:extend' (e.g. the blockquote tint) then fills the wrapped
line to the window edge instead of stopping at the last word."
  (let ((f (and (> (length line) 0)
                (get-text-property (1- (length line)) 'face line))))
    (if f (propertize "\n" 'face f) "\n")))

(defun thb-md-render--pixel-wrap (s budget cont-prefix)
  "Word-wrap propertized string S to pixel width <= BUDGET.
Line 1 keeps S's own leading indent, including quote bars and the spaces
between them; continuation lines are prefixed with
CONT-PREFIX (the line's hang indent).  Returns one propertized string with
embedded newlines.  Breaks at ASCII spaces; a single over-long word is left
to overflow (it simply truncates off the right).  Properties are preserved.

Each word and inserted separator is measured once.  Widths are accumulated
instead of repeatedly concatenating and measuring the growing line, making
the wrapping pass linear in the amount of prose."
  (let* ((lead-len (progn
                     (string-match
                      (concat "\\`\\(?:"
                              (regexp-quote thb-md-render-blockquote-prefix)
                              "\\| \\)*")
                      s)
                     (match-end 0)))
         (lead (substring s 0 lead-len))
         (words (thb-md-render--split-words (substring s lead-len))))
    (if (null words)
        s
      (let ((parts (list lead))
            (width (thb-md-render--string-pixel-width lead))
            (cont-prefix-width
             (thb-md-render--string-pixel-width cont-prefix))
            (has-word nil)
            (last-word nil))
        (dolist (word words)
          (let* ((space (and has-word (thb-md-render--sep-space word)))
                 (word-width (thb-md-render--string-pixel-width word))
                 (space-width (if space
                                  (thb-md-render--space-width space)
                                0))
                 (candidate-width (+ width space-width word-width)))
            (if (or (not has-word) (<= candidate-width budget))
                (progn
                  (when space (push space parts))
                  (push word parts)
                  (setq width candidate-width
                        has-word t
                        last-word word))
              (push (thb-md-render--faced-newline last-word) parts)
              (push cont-prefix parts)
              (push word parts)
              (setq width (+ cont-prefix-width word-width)
                    last-word word))))
        (apply #'concat (nreverse parts))))))

(defun thb-md-render--rewrap-prose ()
  "Hard-wrap prose lines to the body width so they read fine in a buffer
that truncates long lines.  Lines tagged `thb-md-render--nowrap' (code
blocks) and blank lines are left alone, so they keep extending to the
right.  Thematic-break rules are redrawn and tables laid out to the
budget.  Continuation lines
inherit the `wrap-prefix' of each line's last character as hang indent.
Run once, after the whole document has been emitted."
  (let* ((win (thb-md-render--wrapping-window))
         (thb-md-render--measure-frame
          (if win (window-frame win) (selected-frame)))
         (thb-md-render--space-widths (make-hash-table :test 'equal))
         (budget (thb-md-render--prose-budget-px)))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((bol (line-beginning-position))
              (eol (line-end-position)))
          (cond
           ((= bol eol)
            (goto-char eol))
           ((text-property-any bol eol 'thb-md-render--rule t)
            (thb-md-render--resize-rule bol eol budget)
            (goto-char (line-end-position)))
           ((thb-md-render--property-start bol eol 'thb-md-render--table)
            ;; Leave point on the table's last line for `forward-line'.
            (goto-char (1- (thb-md-render--layout-table bol eol budget))))
           ;; A quote bar inserted at bol carries no tag, so search the line.
           ((text-property-any bol eol 'thb-md-render--nowrap t)
            (goto-char eol))
           (t
            (let ((line (buffer-substring bol eol)))
              (if (<= (thb-md-render--string-pixel-width line) budget)
                  (goto-char eol)
                ;; Bullets and quote bars at bol carry a shorter (or no) hang
                ;; than the content, so read the hang from the line's end.
                (let* ((prefix (or (get-text-property (1- eol) 'wrap-prefix)
                                   ""))
                       (wrapped (thb-md-render--pixel-wrap line budget prefix)))
                  (delete-region bol eol)
                  (goto-char bol)
                  (insert wrapped)))))))
        (forward-line 1)))))

(defun thb-md-render--property-start (bol eol prop)
  "Return the first position in BOL..EOL where PROP is non-nil, or nil."
  (if (get-text-property bol prop)
      bol
    (let ((pos (next-single-property-change bol prop nil eol)))
      (and (< pos eol) pos))))

(defun thb-md-render--resize-rule (bol eol budget)
  "Redraw the thematic-break rule on line BOL..EOL to fill BUDGET."
  (let* ((rule-start (text-property-any bol eol 'thb-md-render--rule t))
         (rule-end (next-single-property-change
                    rule-start 'thb-md-render--rule nil eol))
         (props (text-properties-at rule-start))
         (lead-width (thb-md-render--string-pixel-width
                      (buffer-substring bol rule-start)))
         (glyph-width (max 1 (thb-md-render--string-pixel-width
                              (buffer-substring rule-start (1+ rule-start)))))
         (count (max 1 (floor (- budget lead-width) glyph-width))))
    (delete-region rule-start rule-end)
    (goto-char rule-start)
    (insert (apply #'propertize (make-string count ?─) props))))

(defun thb-md-render--capture-view (win)
  "Return point and WIN's start as fractions of the buffer, or nil.
Point is WIN's point when WIN shows the current buffer."
  (let ((span (- (point-max) (point-min)))
        (shown (and (window-live-p win)
                    (eq (window-buffer win) (current-buffer)))))
    (when (> span 0)
      (cl-flet ((frac (pos) (/ (float (- pos (point-min))) span)))
        (cons (frac (if shown (window-point win) (point)))
              (and shown (frac (window-start win))))))))

(defun thb-md-render--restore-view (view win)
  "Move point and WIN's start to the lines nearest the fractions in VIEW."
  (when view
    (cl-flet ((line-at (frac)
                (save-excursion
                  (goto-char (+ (point-min)
                                (round (* frac (- (point-max) (point-min))))))
                  (line-beginning-position))))
      (goto-char (line-at (car view)))
      (when (window-live-p win)
        (set-window-point win (point))
        (when (cdr view)
          (set-window-start win (line-at (cdr view)) t))))))

(defun thb-md-render--rewrap-cached-content ()
  "Restore and rewrap `thb-md-render--unwrapped-content' without parsing.
Preserve point's and the owner window's start's proportional document
positions across the width-dependent representation change."
  (when thb-md-render--unwrapped-content
    (let* ((win (thb-md-render--wrapping-window))
           (view (thb-md-render--capture-view win))
           (inhibit-read-only t))
      (erase-buffer)
      (insert thb-md-render--unwrapped-content)
      (thb-md-render--apply-olivetti-width)
      (thb-md-render--rewrap-prose)
      (setq thb-md-render--wrap-width (thb-md-render--window-width-px))
      (thb-md-render--restore-view view win))))

(defun thb-md-render--reflow-now ()
  "Perform one width-only reflow when the owner window width changed."
  (let ((width (thb-md-render--window-width-px)))
    (when (and width thb-md-render--unwrapped-content
               (not (equal width thb-md-render--wrap-width)))
      (let ((thb-md-render--reflowing t))
        (thb-md-render--rewrap-cached-content)))))

(defun thb-md-render--maybe-reflow ()
  "Debounce a width-only reflow for this preview's stable owner window.
Margin-reset configuration hooks are ignored during reflow.  Reflow restores
the cached emitted document, so it does not reread or reparse source."
  (when (and thb-md-render--source-file
             thb-md-render--unwrapped-content
             (not thb-md-render--reflowing))
    (when (timerp thb-md-render--reflow-timer)
      (cancel-timer thb-md-render--reflow-timer))
    (setq thb-md-render--reflow-timer
          (run-with-idle-timer
           0.2 nil
           (lambda (buf)
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (setq thb-md-render--reflow-timer nil)
                 (thb-md-render--reflow-now))))
           (current-buffer)))))

(defun thb-md-render--apply-olivetti-width ()
  "Size the olivetti reading column for the current window.
Keeps the centered reading column at most `thb-md-render-body-width'
columns wide on a wide window, but FILLS a narrower window instead of
leaving big margins.  Done as a window *fraction*, because olivetti's
integer column sizing mis-computes margins under `text-scale' (a 140-col
body came out ~100 cols with 10-col margins in a 120-col frame).  A float
`thb-md-render-body-width' in (0,1] is passed through as an explicit
fraction."
  (when (and (bound-and-true-p olivetti-mode)
             (numberp thb-md-render-body-width)
             (> thb-md-render-body-width 0))
    (let ((frac
           (if (and (floatp thb-md-render-body-width)
                    (<= thb-md-render-body-width 1.0))
               thb-md-render-body-width
             (let* ((win (thb-md-render--wrapping-window))
                    (total (and win (window-total-width win))))
               (and total (> total 0)
                    (min 1.0 (/ (float thb-md-render-body-width) total)))))))
      (when frac
        (setq-local olivetti-body-width frac)
        ;; Not `olivetti-reset-all-windows': that only REMOVES margins, so
        ;; prose would then wrap to the full window width.
        (when (fboundp 'olivetti-set-buffer-windows)
          (ignore-errors (olivetti-set-buffer-windows)))))))

;;;; Navigation ---------------------------------------------------------

(defun thb-md-render--heading-at (pos)
  "Return the (LEVEL ANCHOR TITLE) of the heading at POS, or nil."
  (get-text-property pos 'thb-md-render--heading))

(defun thb-md-render--headings ()
  "Return the preview's headings as ((POS LEVEL ANCHOR TITLE) ...).
A heading wrapped onto several lines appears once, at its start."
  (let ((pos (point-min)) last headings)
    (while (< pos (point-max))
      (let ((heading (thb-md-render--heading-at pos)))
        (when (and heading (not (eq heading last)))
          (push (cons pos heading) headings)
          (setq last heading)))
      (setq pos (next-single-property-change pos 'thb-md-render--heading
                                             nil (point-max))))
    (nreverse headings)))

(defun thb-md-render--current-heading (headings)
  "Return the entry of HEADINGS at or before point, or nil."
  (car (last (seq-filter (lambda (h) (<= (car h) (point))) headings))))

(defun thb-md-render--goto-heading (heading)
  "Move to HEADING, an entry of `thb-md-render--headings'."
  (goto-char (car heading)))

(defun thb-md-render-next-heading ()
  "Move to the next heading."
  (interactive)
  (thb-md-render--goto-heading
   (or (seq-find (lambda (h) (> (car h) (point))) (thb-md-render--headings))
       (user-error "No next heading"))))

(defun thb-md-render-previous-heading ()
  "Move to the previous heading."
  (interactive)
  (thb-md-render--goto-heading
   (or (car (last (seq-filter (lambda (h) (< (car h) (point)))
                              (thb-md-render--headings))))
       (user-error "No previous heading"))))

(defun thb-md-render-up-heading ()
  "Move to the parent of the current heading."
  (interactive)
  (let* ((headings (thb-md-render--headings))
         (current (or (thb-md-render--current-heading headings)
                      (user-error "Not under a heading"))))
    (thb-md-render--goto-heading
     (or (car (last (seq-filter (lambda (h) (and (< (car h) (car current))
                                                 (< (nth 1 h) (nth 1 current))))
                                headings)))
         (user-error "No parent heading")))))

(defun thb-md-render--same-level (backward)
  "Move to the next same-level heading, or the previous one if BACKWARD.
Stop at a heading of a higher level: it ends the current subtree."
  (let* ((headings (thb-md-render--headings))
         (current (or (thb-md-render--current-heading headings)
                      (user-error "Not under a heading")))
         (level (nth 1 current))
         (candidates (if backward
                         (reverse (seq-filter (lambda (h) (< (car h) (car current)))
                                              headings))
                       (seq-filter (lambda (h) (> (car h) (car current))) headings)))
         (target (seq-find (lambda (h) (<= (nth 1 h) level)) candidates)))
    (if (and target (= (nth 1 target) level))
        (thb-md-render--goto-heading target)
      (user-error "No %s heading at this level" (if backward "previous" "next")))))

(defun thb-md-render-forward-same-level ()
  "Move to the next heading at the current heading's level."
  (interactive)
  (thb-md-render--same-level nil))

(defun thb-md-render-backward-same-level ()
  "Move to the previous heading at the current heading's level."
  (interactive)
  (thb-md-render--same-level t))

(defun thb-md-render--imenu-index ()
  "Return an imenu index of headings, each named by its path of titles."
  (let (stack index)
    (pcase-dolist (`(,pos ,level ,_anchor ,title) (thb-md-render--headings))
      (while (and stack (>= (caar stack) level))
        (pop stack))
      (push (cons level title) stack)
      (push (cons (mapconcat #'cdr (reverse stack) " / ") pos) index))
    (nreverse index)))

(defun thb-md-render--link-starts ()
  "Return the start positions of the preview's links, in order."
  (let ((pos (point-min)) starts)
    (while (< pos (point-max))
      (when (and (get-text-property pos 'thb-md-link)
                 (or (= pos (point-min))
                     (not (eq (get-text-property (1- pos) 'thb-md-link)
                              (get-text-property pos 'thb-md-link)))))
        (push pos starts))
      (setq pos (next-single-property-change pos 'thb-md-link nil (point-max))))
    (nreverse starts)))

(defun thb-md-render-next-link ()
  "Move to the start of the next link."
  (interactive)
  (goto-char (or (seq-find (lambda (p) (> p (point))) (thb-md-render--link-starts))
                 (user-error "No next link"))))

(defun thb-md-render-previous-link ()
  "Move to the start of the previous link."
  (interactive)
  (goto-char (or (car (last (seq-filter (lambda (p) (< p (point)))
                                        (thb-md-render--link-starts))))
                 (user-error "No previous link"))))

(defun thb-md-render--decode-url (string)
  "Return percent-encoded STRING decoded as UTF-8."
  (decode-coding-string (url-unhex-string string) 'utf-8))

(defun thb-md-render--goto-anchor (anchor)
  "Move to the heading whose anchor is ANCHOR, compared case-insensitively."
  (let ((target (seq-find (lambda (h) (equal (nth 2 h) (downcase anchor)))
                          (thb-md-render--headings))))
    (unless target
      (user-error "No heading for #%s" anchor))
    (thb-md-render--goto-heading target)
    ;; Put the heading at the top, as a document viewer does.
    (when (eq (window-buffer) (current-buffer))
      (recenter 0))))

(defun thb-md-render--follow-file (url)
  "Open local file URL, relative to the rendered source's directory.
A markdown target opens in its own preview, at the heading its fragment
names; any other file opens in Emacs, at line N for a #LN fragment."
  (let* ((hash (string-search "#" url))
         (path (if hash (substring url 0 hash) url))
         (fragment (and hash (thb-md-render--decode-url (substring url (1+ hash)))))
         (path (if (string-prefix-p "file:" path)
                   (url-filename (url-generic-parse-url path))
                 path))
         (file (expand-file-name (thb-md-render--decode-url path)
                                 (file-name-directory thb-md-render--source-file)))
         (line (and fragment
                    (string-match "\\`L\\([0-9]+\\)\\(?:-L[0-9]+\\)?\\'" fragment)
                    (string-to-number (match-string 1 fragment)))))
    (unless (file-exists-p file)
      (user-error "Link target does not exist: %s" file))
    (xref-push-marker-stack)
    (if (and (not line)
             (let ((case-fold-search t))
               (string-match-p "\\.\\(?:md\\|markdown\\|mdown\\|mkd\\)\\'" file)))
        (progn
          (thb-md-render--show-preview file)
          (when (and fragment (not (string-empty-p fragment)))
            (thb-md-render--goto-anchor fragment)))
      (find-file file)
      (when line
        (goto-char (point-min))
        (forward-line (1- line))))))

(defun thb-md-render--follow-url (url)
  "Follow URL: a #heading here, a local file, or anything else in a browser.
Jumps inside Emacs push the xref marker stack, so \\[xref-go-back] returns."
  (cond
   ((string-prefix-p "#" url)
    (let ((anchor (thb-md-render--decode-url (substring url 1))))
      ;; Check before pushing, so a bad anchor leaves no stray marker.
      (unless (seq-find (lambda (h) (equal (nth 2 h) (downcase anchor)))
                        (thb-md-render--headings))
        (user-error "No heading for #%s" anchor))
      (xref-push-marker-stack)
      (thb-md-render--goto-anchor anchor)))
   ((or (string-prefix-p "file:" url)
        (not (string-match-p "\\`\\(?:[[:alpha:]][[:alnum:]+.-]*:\\|//\\)" url)))
    (thb-md-render--follow-file url))
   (t (browse-url url))))

(defun thb-md-render-follow-link (&optional pos)
  "Follow the link at POS, or at point."
  (interactive)
  (thb-md-render--follow-url
   (or (get-text-property (or pos (point)) 'thb-md-link)
       (user-error "No link at point"))))

(defun thb-md-render-follow-link-at-mouse (event)
  "Follow the link clicked in EVENT."
  (interactive "e")
  (let ((posn (event-start event)))
    (select-window (posn-window posn))
    (goto-char (posn-point posn))
    (thb-md-render-follow-link)))

;;;; Mode for the rendered preview buffer ---------------------------------

(defvar thb-md-render-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "g") #'thb-md-render-revert)
    (define-key map (kbd "RET") #'thb-md-render-follow-link)
    (define-key map (kbd "C-c C-o") #'thb-md-render-follow-link)
    (define-key map (kbd "TAB") #'thb-md-render-next-link)
    (define-key map (kbd "<backtab>") #'thb-md-render-previous-link)
    (define-key map (kbd "n") #'thb-md-render-next-heading)
    (define-key map (kbd "p") #'thb-md-render-previous-heading)
    (define-key map (kbd "u") #'thb-md-render-up-heading)
    ;; markdown-mode's outline keys.
    (define-key map (kbd "C-c C-n") #'thb-md-render-next-heading)
    (define-key map (kbd "C-c C-p") #'thb-md-render-previous-heading)
    (define-key map (kbd "C-c C-u") #'thb-md-render-up-heading)
    (define-key map (kbd "C-c C-f") #'thb-md-render-forward-same-level)
    (define-key map (kbd "C-c C-b") #'thb-md-render-backward-same-level)
    map)
  "Keymap for `thb-md-render-mode'.")

;; Render buffers are read-only views; evil's default `normal' state binds
;; `q' to `evil-record-macro' which shadows the mode-map's `q -> quit-window'.
;; `motion' state has the same navigation keys but no insert/record entries,
;; so `q' in motion falls through to our local binding.  Motion state still
;; shadows the other mode-map keys, so give them evil-style equivalents.
(declare-function evil-set-initial-state "evil-core")
(declare-function evil-define-key "evil-core")
(with-eval-after-load 'evil
  (evil-set-initial-state 'thb-md-render-mode 'motion)
  (evil-define-key 'motion thb-md-render-mode-map
    (kbd "RET") #'thb-md-render-follow-link
    (kbd "TAB") #'thb-md-render-next-link
    (kbd "<backtab>") #'thb-md-render-previous-link
    "]]" #'thb-md-render-next-heading
    "[[" #'thb-md-render-previous-heading
    "gj" #'thb-md-render-forward-same-level
    "gk" #'thb-md-render-backward-same-level
    "gh" #'thb-md-render-up-heading))

(define-derived-mode thb-md-render-mode special-mode "MD-Render"
  "Major mode for rendered markdown preview buffers."
  ;; Truncate long logical lines (no visual wrapping).  Code lines and table
  ;; rows deliberately stay one logical line each, so code (and a table too
  ;; wide even at minimum column widths) extends to the right and scrolls
  ;; horizontally instead of folding.  PROSE is wrapped
  ;; into real lines by `thb-md-render--rewrap-prose' at render time (Emacs
  ;; has no per-line truncation), so paragraphs / lists / quotes still read
  ;; at the body width even though the buffer truncates.
  (setq-local truncate-lines t)
  (setq-local word-wrap nil)
  (visual-line-mode -1)
  (setq-local left-fringe-width 0)
  (setq-local right-fringe-width 0)
  (when-let* ((w (get-buffer-window (current-buffer))))
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
    (setq-local olivetti-style nil)
    (olivetti-mode 1)
    ;; Size the reading column as a window fraction (see the function):
    ;; olivetti's integer column sizing leaves large margins under
    ;; `text-scale', making prose look cramped in a narrow frame.
    (thb-md-render--apply-olivetti-width))
  ;; olivetti-mode-on-hook turns on `visual-line-mode' in this config, which
  ;; flips `truncate-lines' back off and soft-wraps every long line.  Re-assert
  ;; truncation AFTER olivetti has run so tables and code stay on one logical
  ;; line and extend off to the right (scroll horizontally) instead of folding.
  (visual-line-mode -1)
  (setq-local truncate-lines t)
  (setq-local word-wrap nil)
  ;; Re-wrap prose to the new width when the window is first displayed or
  ;; resized, so prose neither clips (budget too wide) nor under-fills.
  (add-hook 'window-configuration-change-hook
            #'thb-md-render--maybe-reflow nil t)
  ;; Timers can outlive a buffer unless they are cancelled explicitly.  Keep
  ;; cleanup on every render buffer, even ones that are never file-watched.
  (add-hook 'kill-buffer-hook #'thb-md-render--cleanup nil t)
  (setq-local imenu-create-index-function #'thb-md-render--imenu-index)
  (buffer-disable-undo))

;;;; Entry points -------------------------------------------------------

(defvar-local thb-md-render--watch nil
  "`file-notify' descriptor for the source file; re-renders on change.")

(defvar-local thb-md-render--watch-timer nil
  "Debounce or reattachment timer for `thb-md-render--watch'.")

(defcustom thb-md-render-watch-debounce 0.1
  "Seconds to debounce file-notify events before refreshing a preview."
  :type 'number
  :group 'thb-md-render)

(defvar-local thb-md-render--watch-retry-delay nil
  "Seconds before the next reattachment attempt while the source is absent.
Doubles per failed attempt up to `thb-md-render--watch-retry-max'; nil once
the source is readable again.")

(defconst thb-md-render--watch-retry-max 5.0
  "Longest delay in seconds between reattachment attempts.")

(defun thb-md-render--canonical-source-file (path)
  "Return the canonical absolute identity for markdown source PATH."
  (file-truename (expand-file-name path)))

(defun thb-md-render--preview-buffer-name (source-file)
  "Return the unique preview buffer name for canonical SOURCE-FILE."
  (format "*md render: %s*" source-file))

(defun thb-md-render-file (path)
  "Parse PATH into its source-specific markdown preview buffer.
When called interactively, also display the rendered buffer, before
rendering so the first prose wrap uses the live window width.  Lisp calls
return the rendered buffer without displaying it."
  (interactive "fMarkdown file: ")
  (let* ((interactivep (called-interactively-p 'interactive))
         (path (thb-md-render--canonical-source-file path))
         (buf-name (thb-md-render--preview-buffer-name path))
         (out (get-buffer-create buf-name))
         (src (generate-new-buffer (format " *thb-md-source: %s*"
                                           (file-name-nondirectory path))
                                   t))
         (inline-src
          (generate-new-buffer (format " *thb-md-inline: %s*"
                                       (file-name-nondirectory path)) t))
         (table-scratch
          (generate-new-buffer (format " *thb-md-table-cells: %s*"
                                       (file-name-nondirectory path)) t))
         markdown-parser
         inline-parser)
    (when interactivep
      (pop-to-buffer out))
    (unwind-protect
        (progn
          ;; Parse blocks from the source, then inline content from a copy
          ;; whose block-continuation markers are blanked (see
          ;; `thb-md-render--setup-parsers').
          (with-current-buffer src
            (insert-file-contents path)
            (let ((text (buffer-string)))
              (with-current-buffer inline-src (insert text)))
            (pcase-let ((`(,host . ,embedded)
                         (thb-md-render--setup-parsers inline-src)))
              (setq markdown-parser host
                    inline-parser embedded)))
          (let ((root (treesit-parser-root-node markdown-parser)))
            (with-current-buffer out
              (unless (derived-mode-p 'thb-md-render-mode)
                (thb-md-render-mode))
              (let ((inhibit-read-only t))
                (erase-buffer)
                (let* ((thb-md-render--src-buffer inline-src)
                       (thb-md-render--inline-parser inline-parser)
                       (thb-md-render--table-cell-buffer table-scratch)
                       (thb-md-render--link-definitions
                        (thb-md-render--collect-link-definitions root))
                       (thb-md-render--anchor-counts
                        (make-hash-table :test 'equal)))
                  (thb-md-render--walk root))
                ;; Cache the fully emitted, propertized document before the
                ;; width-dependent pass.  Resizes can restore this snapshot
                ;; and rewrap without rereading or reparsing the source.
                (setq thb-md-render--unwrapped-content
                      (buffer-substring (point-min) (point-max)))
                ;; Size the reading column to the owner window first, then
                ;; wrap prose to that width (tables / code are skipped via
                ;; `thb-md-render--nowrap').
                (thb-md-render--apply-olivetti-width)
                (thb-md-render--rewrap-prose)
                (setq thb-md-render--wrap-width
                      (thb-md-render--window-width-px))
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
      (dolist (scratch (list src inline-src table-scratch))
        (when (buffer-live-p scratch)
          (kill-buffer scratch))))
    out))

(defun thb-md-render-revert ()
  "Reparse and re-render the current preview buffer from its source file.
Keeps point and the window start at roughly the same place when an explicit
refresh or source-file notification replaces the emitted document."
  (interactive)
  (unless thb-md-render--source-file
    (user-error "Not a markdown render buffer"))
  (let* ((source thb-md-render--source-file)
         (win (thb-md-render--wrapping-window))
         (view (thb-md-render--capture-view win)))
    (let ((buf (thb-md-render-file source)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (thb-md-render--restore-view view win))))))

(defun thb-md-render--cancel-watch-timer ()
  "Cancel the current buffer's pending file-watch timer, if any."
  (when (timerp thb-md-render--watch-timer)
    (cancel-timer thb-md-render--watch-timer))
  (setq thb-md-render--watch-timer nil))

(defun thb-md-render--remove-watch ()
  "Remove the current buffer's file-notify descriptor, if any."
  (when thb-md-render--watch
    (ignore-errors (file-notify-rm-watch thb-md-render--watch))
    (setq thb-md-render--watch nil)))

(defun thb-md-render--run-watch-update (preview-buffer reattach)
  "Refresh PREVIEW-BUFFER, first replacing its watch when REATTACH is non-nil."
  (when (buffer-live-p preview-buffer)
    (with-current-buffer preview-buffer
      (setq thb-md-render--watch-timer nil)
      (if (file-readable-p thb-md-render--source-file)
          (progn
            (setq thb-md-render--watch-retry-delay nil)
            (when reattach
              ;; Always dispose the old descriptor before taking ownership of
              ;; a replacement.  This is harmless when a terminal event already
              ;; cleared it and protects direct callers from duplicate watches.
              (thb-md-render--remove-watch)
              (setq thb-md-render--watch
                    (thb-md-render--setup-watch
                     preview-buffer thb-md-render--source-file)))
            (thb-md-render-revert))
        ;; Atomic replacements normally expose the new path immediately, but
        ;; keep one owned retry timer while it is absent rather than silently
        ;; leaving a live preview unwatched.  Back off so a source that stays
        ;; deleted costs one check per `thb-md-render--watch-retry-max'.
        (when reattach
          (setq thb-md-render--watch-retry-delay
                (min thb-md-render--watch-retry-max
                     (max 0.1 (* 2 (or thb-md-render--watch-retry-delay
                                       thb-md-render-watch-debounce)))))
          (thb-md-render--schedule-watch-update
           preview-buffer t thb-md-render--watch-retry-delay))))))

(defun thb-md-render--schedule-watch-update (preview-buffer reattach &optional delay)
  "Refresh PREVIEW-BUFFER after DELAY seconds (default: the debounce).
When REATTACH is non-nil, replace its file watch immediately before refresh."
  (when (buffer-live-p preview-buffer)
    (with-current-buffer preview-buffer
      (thb-md-render--cancel-watch-timer)
      (setq thb-md-render--watch-timer
            (run-at-time (or delay thb-md-render-watch-debounce) nil
                         #'thb-md-render--run-watch-update
                         preview-buffer reattach)))))

(defun thb-md-render--setup-watch (preview-buffer source-file)
  "Create a file-notify watch owned by PREVIEW-BUFFER for SOURCE-FILE."
  (file-notify-add-watch
   source-file '(change attribute-change)
   (lambda (event)
     (condition-case err
         (pcase-let ((`(,descriptor ,action . ,_rest) event))
           (when (buffer-live-p preview-buffer)
             (with-current-buffer preview-buffer
               ;; A removed descriptor can still have queued events.  Ignore
               ;; those so an obsolete watch cannot disturb its replacement.
               (when (equal descriptor thb-md-render--watch)
                 (cond
                  ((memq action '(stopped deleted renamed))
                   (thb-md-render--remove-watch)
                   (thb-md-render--schedule-watch-update preview-buffer t))
                  ((memq action '(changed attribute-changed created))
                   (thb-md-render--schedule-watch-update preview-buffer nil)))))))
       (error (message "thb-md-render watch error: %s" err))))))

(defun thb-md-render--ensure-watch (preview-buffer)
  "Ensure PREVIEW-BUFFER owns one watch and its cleanup hook."
  (with-current-buffer preview-buffer
    (add-hook 'kill-buffer-hook #'thb-md-render--cleanup nil t)
    (unless thb-md-render--watch
      (setq thb-md-render--watch
            (thb-md-render--setup-watch
             preview-buffer thb-md-render--source-file)))))

(defun thb-md-render--cleanup ()
  "Remove file-notify resources and timers owned by the current buffer."
  (thb-md-render--remove-watch)
  (thb-md-render--cancel-watch-timer)
  (when (timerp thb-md-render--reflow-timer)
    (cancel-timer thb-md-render--reflow-timer))
  (setq thb-md-render--reflow-timer nil))

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
    (let ((src (buffer-file-name))
          (source-buffer (current-buffer)))
      (unless src (user-error "Buffer is not visiting a file"))
      (when (buffer-modified-p) (save-buffer))
      (condition-case err
          (thb-md-render--show-preview src)
        (error (switch-to-buffer source-buffer)
               (signal (car err) (cdr err))))))
   (t (user-error "Not in markdown-ts-mode or a render preview"))))

(defun thb-md-render--show-preview (source-file)
  "Render SOURCE-FILE into its watched preview in the selected window.
Return the preview buffer."
  ;; Show the preview before rendering so the first prose wrap uses the
  ;; live window width; rendering hidden would wrap to a fallback width
  ;; and then wrap the whole document again on first display.
  (switch-to-buffer
   (get-buffer-create (thb-md-render--preview-buffer-name
                       (thb-md-render--canonical-source-file source-file))))
  (let ((preview (thb-md-render-file source-file)))
    (with-current-buffer preview
      ;; Rendering just read the latest contents, so any queued refresh is
      ;; redundant.  Reused previews still pass through this path and regain
      ;; lifecycle hooks after a live code reload.
      (thb-md-render--cancel-watch-timer)
      (thb-md-render--ensure-watch preview))
    (switch-to-buffer preview)
    ;; Defensive: re-apply visual tweaks AFTER the buffer is shown in a
    ;; window.  Mode setup runs before the buffer has a window, so
    ;; `set-window-fringes' and global-mode hooks can race and reset what
    ;; mode init tried to set.
    (set-window-fringes (selected-window) 0 0)
    (display-line-numbers-mode -1)
    preview))

(provide 'thb-markdown-render)
;;; thb-markdown-render.el ends here
