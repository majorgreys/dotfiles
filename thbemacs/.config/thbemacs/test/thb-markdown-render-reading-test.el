;;; thb-markdown-render-reading-test.el --- Renderer properties and examples -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Property tests over generated markdown, plus example tests for block
;; combinations and paths the terminal regression file does not reach.
;; Generated documents use unique tokens (w1, w2, ...) so content order
;; can be checked regardless of markup and wrapping.
;;
;; Run from this repository's thbemacs/.config/thbemacs directory, with the
;; installed grammars on the path:
;;   emacs -Q --batch --eval '(setq treesit-extra-load-path (list (expand-file-name "~/.config/thbemacs/tree-sitter")))' \
;;     -l ert -l test/thb-markdown-render-reading-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "../lisp/thb-markdown-render.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)
(require 'markdown-ts-mode)

(defmacro thb-md-test--rendering (markdown &rest body)
  "Render MARKDOWN with neutral settings and run BODY in the preview."
  (declare (indent 1))
  `(let ((path (make-temp-file "thb-md-test-" nil ".md" ,markdown))
         (thb-md-render-text-scale 0)
         (thb-md-render-body-width nil)
         preview)
     (unwind-protect
         (progn
           (setq preview (thb-md-render-file path))
           (with-current-buffer preview ,@body))
       (when (buffer-live-p preview) (kill-buffer preview))
       (delete-file path))))

(defmacro thb-md-test--at-columns (columns &rest body)
  "Run BODY with a terminal window body COLUMNS wide."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'window-body-width)
              (lambda (&rest _) ,columns))
             ((symbol-function 'display-graphic-p) (lambda (&optional _) nil)))
     ,@body))

;;;; Generator ----------------------------------------------------------

(defvar thb-md-test--token 0
  "Counter for unique generated tokens.")

(defun thb-md-test--pick (&rest choices)
  "Return one of CHOICES at random."
  (nth (random (length choices)) choices))

(defun thb-md-test--word ()
  "Return a fresh token, sometimes wrapped in inline markup."
  (let ((w (format "w%d" (cl-incf thb-md-test--token))))
    (pcase (random 8)
      (0 (format "*%s*" w))
      (1 (format "**%s**" w))
      (2 (format "`%s`" w))
      (3 (format "[%s](https://example.com/x)"
                 (if (zerop (random 2)) (format "*%s*" w) w)))
      (_ w))))

(defun thb-md-test--words (min max)
  "Return MIN to MAX generated words joined by spaces."
  (mapconcat #'identity
             (cl-loop repeat (+ min (random (1+ (- max min))))
                      collect (thb-md-test--word))
             " "))

(defun thb-md-test--paragraph ()
  "Return paragraph lines; adjacent lines are soft or hard breaks."
  (let ((lines (cl-loop repeat (1+ (random 3))
                        collect (thb-md-test--words 2 14))))
    (cl-loop for (line . rest) on lines
             collect (if (and rest (zerop (random 3))) (concat line "\\") line))))

(defun thb-md-test--code ()
  "Return fenced code lines, sometimes wider than any test window."
  (append '("```text")
          (cl-loop repeat (1+ (random 3))
                   collect (mapconcat (lambda (_) (format "w%d" (cl-incf thb-md-test--token)))
                                      (number-sequence 1 (1+ (random 30))) " "))
          '("```")))

(defun thb-md-test--table ()
  "Return pipe-table lines with tokens in every cell."
  (let ((cols (+ 2 (random 2))))
    (cl-flet ((row () (concat "| " (mapconcat (lambda (_) (thb-md-test--words 1 (if (zerop (random 3)) 12 2)))
                                              (number-sequence 1 cols) " | ")
                              " |")))
      (append (list (row)
                    (concat "|" (mapconcat (lambda (_) " --- ") (number-sequence 1 cols) "|") "|"))
              (cl-loop repeat (1+ (random 3)) collect (row))))))

(defun thb-md-test--indent (lines prefix &optional first-prefix)
  "Prefix LINES with PREFIX, the first with FIRST-PREFIX when given.
Blank lines get the prefix trimmed of trailing spaces."
  (cl-loop for line in lines
           for i from 0
           for p = (if (and first-prefix (= i 0)) first-prefix prefix)
           collect (if (string-empty-p line) (string-trim-right p) (concat p line))))

(defun thb-md-test--join-blocks (blocks)
  "Join BLOCKS (lists of lines) with blank lines between them."
  (cl-loop for (block . rest) on blocks
           append block
           when rest collect ""))

(defun thb-md-test--list (depth)
  "Return list lines nested at most DEPTH more levels."
  (let ((ordered (zerop (random 2))))
    (cl-loop for n from 1 to (1+ (random 3))
             append
             (let* ((marker (if ordered (format "%d. " n) "- "))
                    (pad (make-string (length marker) ?\s))
                    (body (append (thb-md-test--paragraph)
                                  (when (and (> depth 0) (zerop (random 3)))
                                    (cons "" (thb-md-test--block (1- depth) t))))))
               (thb-md-test--indent body pad marker)))))

(defun thb-md-test--block (depth &optional nested)
  "Return the lines of one random block, nesting at most DEPTH levels.
NESTED blocks exclude headings and rules, which only appear top-level."
  (pcase (random (if nested 5 7))
    (0 (thb-md-test--paragraph))
    (1 (thb-md-test--code))
    (2 (thb-md-test--table))
    (3 (if (> depth 0) (thb-md-test--list depth) (thb-md-test--paragraph)))
    (4 (if (> depth 0)
           (thb-md-test--indent
            (thb-md-test--join-blocks
             (cl-loop repeat (1+ (random 2))
                      collect (thb-md-test--block (1- depth) t)))
            "> ")
         (thb-md-test--paragraph)))
    (5 (list (concat (make-string (1+ (random 3)) ?#) " " (thb-md-test--words 1 5))))
    (_ (list "---"))))

(defun thb-md-test--document (seed)
  "Return a random markdown document for SEED."
  (random (format "thb-md-%d" seed))
  (setq thb-md-test--token 0)
  (let ((blocks (cl-loop repeat (+ 2 (random 5))
                         collect (thb-md-test--block 3))))
    ;; A leading `---' opens YAML front matter, which is not rendered.
    (when (equal (car blocks) '("---"))
      (setcar blocks (thb-md-test--paragraph)))
    (concat (mapconcat #'identity (thb-md-test--join-blocks blocks) "\n")
            "\n")))

(ert-deftest thb-md-render-inline-markup-does-not-span-blocks ()
  (thb-md-test--rendering "1. a `x` `y`\n2. `z` b\n\nc *d\n\ne* f\n"
    (let ((text (thb-md-test--text)))
      (should (string-match-p "^1\\. a x y\n2\\. z b$" text))
      (should (string-match-p "^c \\*d$" text))
      (should (string-match-p "^e\\* f$" text)))))

(defun thb-md-test--tokens (text)
  "Return the generated tokens in TEXT, in order."
  (let ((pos 0) tokens)
    (while (string-match "\\bw[0-9]+\\b" text pos)
      (push (match-string 0 text) tokens)
      (setq pos (match-end 0)))
    (nreverse tokens)))

(defconst thb-md-test--seeds (number-sequence 1 40)
  "Seeds for the property tests.  Failures report their seed and document.")

(defmacro thb-md-test--for-documents (&rest body)
  "Run BODY in the rendered preview of each seeded document.
`doc' is bound to the markdown source."
  `(dolist (seed thb-md-test--seeds)
     (let ((doc (thb-md-test--document seed)))
       (ert-info ((format "seed %d:\n%s" seed doc))
         (thb-md-test--rendering doc
           (save-window-excursion
             (set-window-buffer (selected-window) (current-buffer))
             ,@body))))))

;;;; Properties ---------------------------------------------------------

(ert-deftest thb-md-render-property-tokens-survive-in-order ()
  (thb-md-test--for-documents
   ;; Wide enough that no table cell wraps: source order is kept exactly.
   (thb-md-test--at-columns 2000
     (thb-md-render--rewrap-cached-content)
     (should (equal (thb-md-test--tokens (buffer-string))
                    (thb-md-test--tokens doc))))
   ;; Narrow: wrapped table cells interleave by line, but nothing is lost.
   (dolist (columns '(40 100))
     (thb-md-test--at-columns columns
       (thb-md-render--rewrap-cached-content)
       (should (equal (sort (thb-md-test--tokens (buffer-string)) #'string<)
                      (sort (thb-md-test--tokens doc) #'string<)))))))

(ert-deftest thb-md-render-property-no-markup-syntax-remains ()
  (thb-md-test--for-documents
   (let ((text (buffer-substring-no-properties (point-min) (point-max))))
     (should-not (string-match-p "[>\\\\*`|#]\\|\\](" text)))))

(defun thb-md-test--quote-bg-holes ()
  "Return (LINE . COLUMNS) for quote lines with cells lacking the quote face.
A quote line is one starting with the bar; every cell on it, and its
newline, must carry `thb-md-render-blockquote' so the tint is unbroken."
  (let (holes)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((bol (line-beginning-position)) (eol (line-end-position)))
          (when (and (< bol eol)
                     (string-prefix-p thb-md-render-blockquote-prefix
                                      (buffer-substring-no-properties bol eol)))
            (let ((missing (cl-loop for p from bol to (min eol (1- (point-max)))
                                    unless (memq 'thb-md-render-blockquote
                                                 (ensure-list (get-text-property p 'face)))
                                    collect (- p bol))))
              (when missing
                (push (cons (buffer-substring-no-properties bol eol) missing) holes)))))
        (forward-line 1)))
    (nreverse holes)))

(ert-deftest thb-md-render-property-quote-background-is-unbroken ()
  (thb-md-test--for-documents
   (dolist (columns '(40 72))
     (thb-md-test--at-columns columns
       (thb-md-render--rewrap-cached-content)
       (ert-info ((format "%d columns" columns))
         (should (equal (thb-md-test--quote-bg-holes) nil)))))))

(ert-deftest thb-md-render-property-prose-fits-width ()
  (thb-md-test--for-documents
   (dolist (columns '(40 72 100))
     (thb-md-test--at-columns columns
       (thb-md-render--rewrap-cached-content)
       (goto-char (point-min))
       (while (not (eobp))
         (let* ((bol (line-beginning-position)) (eol (line-end-position))
                (table-line (and (< bol eol)
                                 (cl-some (lambda (f) (memq f '(thb-md-render-table
                                                                thb-md-render-table-rule)))
                                          (ensure-list (get-text-property (1- eol) 'face))))))
           ;; Prose always fits.  Table lines fit too once the window leaves
           ;; room for the generated tables' columns at minimum width; only
           ;; code is meant to extend to the right.
           (when (or (not (text-property-any bol eol 'thb-md-render--nowrap t))
                     (and table-line (>= columns 72)))
             (ert-info ((format "%d columns, line: %S" columns
                                (buffer-substring-no-properties bol eol)))
               (should (<= (string-width (buffer-substring bol eol))
                           (1- columns))))))
         (forward-line 1))))))

(ert-deftest thb-md-render-property-reflow-matches-fresh-render ()
  (dolist (seed thb-md-test--seeds)
    (let* ((doc (thb-md-test--document seed))
           (path (make-temp-file "thb-md-test-" nil ".md" doc))
           (thb-md-render-text-scale 0)
           (thb-md-render-body-width nil)
           (preview (get-buffer-create (thb-md-render--preview-buffer-name
                                        (thb-md-render--canonical-source-file path)))))
      (ert-info ((format "seed %d:\n%s" seed doc))
        (unwind-protect
            (save-window-excursion
              (set-window-buffer (selected-window) preview)
              (let ((fresh (thb-md-test--at-columns 50
                             (with-current-buffer (thb-md-render-file path)
                               (buffer-string)))))
                (with-current-buffer preview
                  (thb-md-test--at-columns 90 (thb-md-render--rewrap-cached-content))
                  (thb-md-test--at-columns 50 (thb-md-render--rewrap-cached-content))
                  (should (equal-including-properties (buffer-string) fresh)))))
          (kill-buffer preview)
          (delete-file path))))))

;;;; Block combinations --------------------------------------------------

(defun thb-md-test--text ()
  "Return the preview text without properties."
  (buffer-substring-no-properties (point-min) (point-max)))

(ert-deftest thb-md-render-hard-break-in-list-item-hangs ()
  (thb-md-test--rendering "- one\\\n  two\n"
    (should (string-match-p "^• one\n  two$" (thb-md-test--text)))))

(ert-deftest thb-md-render-hard-break-in-quote-keeps-bar ()
  (thb-md-test--rendering "> one\\\n> two\n"
    (should (string-match-p "^▎ one\n▎ two$" (thb-md-test--text)))))

(ert-deftest thb-md-render-table-in-list-item-is-indented ()
  (thb-md-test--rendering "- item\n\n  | a | b |\n  | --- | --- |\n  | c | d |\n"
    (let ((text (thb-md-test--text)))
      (should (string-match-p "^  a  b$" text))
      (should (string-match-p "^  ─+$" text))
      (should (string-match-p "^  c  d$" text)))))

(ert-deftest thb-md-render-table-in-quote-renders-cell-markup ()
  (thb-md-test--rendering "> | *a* | `b` |\n> | --- | --- |\n> | c | d |\n"
    (let ((text (thb-md-test--text)))
      (should (string-match-p "^▎ a  b$" text))
      (should (string-match-p "^▎ c  d$" text)))))

(ert-deftest thb-md-render-ordered-list-in-quote ()
  (thb-md-test--rendering "> 1. a\n> 2. b\n"
    (should (string-match-p "^▎ 1\\. a\n▎ 2\\. b$" (thb-md-test--text)))))

(ert-deftest thb-md-render-nested-ordered-numbering ()
  (thb-md-test--rendering "3. a\n4. b\n   1. c\n   2. d\n5) e\n"
    (let ((text (thb-md-test--text)))
      (should (string-match-p "^3\\. a\n4\\. b\n  1\\. c\n  2\\. d$" text))
      ;; A `)' marker starts a new list, numbered from its own start.
      (should (string-match-p "^5) e$" text)))))

(ert-deftest thb-md-render-repeated-lists-do-not-share-counters ()
  (thb-md-test--rendering "1. a\n2. b\n\npara\n\n1. c\n"
    (should (string-match-p "^1\\. c$" (thb-md-test--text)))))

(ert-deftest thb-md-render-inline-walk-rejects-reentry ()
  ;; Re-pointing the shared inline parser would outdate the outer nodes.
  (let ((thb-md-render--inline-active t))
    (should-error (thb-md-render--inline-walk 1 2))))

(ert-deftest thb-md-render-invalid-numeric-entities-are-replaced ()
  (thb-md-test--rendering "x &#0; &#xD800; &#x110000; y\n"
    (should (string-match-p "^x \uFFFD \uFFFD \uFFFD y$" (thb-md-test--text)))))

(ert-deftest thb-md-render-indented-code-keeps-relative-indent ()
  (dolist (case '(("    top1\n        topdeep\n" . "^top1\n    topdeep$")
                  ("- i\n\n      ind1\n          deeper\n" . "^  ind1\n      deeper$")
                  (">     q1\n>         qdeep\n" . "^▎ q1\n▎     qdeep$")))
    (ert-info ((car case))
      (thb-md-test--rendering (car case)
        (should (string-match-p (cdr case) (thb-md-test--text)))))))

(ert-deftest thb-md-render-indented-code-in-list-has-no-stray-line ()
  (thb-md-test--rendering "- i\n\n      ind1\n      ind2\n\nafter\n"
    (should-not (string-match-p "^ +$" (thb-md-test--text)))))

(ert-deftest thb-md-render-list-paragraph-after-block-has-one-blank-line ()
  (thb-md-test--rendering "- a\n\n  > q\n\n  b\n\n  | t |\n  |--|\n  | x |\n\n  c\n"
    (let ((text (thb-md-test--text)))
      ;; The quote ends on its tinted padding line, then one blank line.
      (should (string-match-p "q\n  ▎ \n\n  b$" text))
      (should (string-match-p "x\n\n  c$" text)))))

(ert-deftest thb-md-render-multiline-code-span-joins-with-one-space ()
  (dolist (case '("- `e\n  f`\n" "> `e\n> f`\n"))
    (ert-info (case)
      (thb-md-test--rendering case
        (should (string-match-p "e f" (thb-md-test--text)))))))

(ert-deftest thb-md-render-quote-has-padding-lines ()
  (dolist (case '(("> a\n" . "\\`▎ \n▎ a\n▎ \n")
                  ("> [!NOTE]\n> a\n" . "\\`▎ \n▎ Note\n▎ a\n▎ \n")
                  ;; Nested quotes pad only at the outermost level.
                  ("> a\n>\n> > b\n" . "\\`▎ \n▎ a\n▎ \n▎ ▎ b\n▎ \n")
                  ;; A code card already pads itself: no doubled padding.
                  ("> ```\n> code\n> ```\n" . "\\`▎ \n▎   code\n▎ \n\n")))
    (ert-info ((car case))
      (thb-md-test--rendering (car case)
        (let ((text (thb-md-test--text)))
          (should (string-match-p (cdr case) text))
          (should-not (string-match-p "^▎ \n▎ \n" text)))
        ;; The padding lines carry the tint across the window.
        (goto-char (point-min))
        (should (memq 'thb-md-render-blockquote
                      (ensure-list (get-text-property (line-end-position) 'face))))))))

(ert-deftest thb-md-render-wrapped-quote-lines-keep-background ()
  (thb-md-test--rendering
      (concat "> " (mapconcat #'identity (make-list 20 "quoted") " ") "\n>\n"
              "> - " (mapconcat #'identity (make-list 20 "listed") " ") "\n"
              ">   - " (mapconcat #'identity (make-list 20 "nested") " ") "\n")
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (thb-md-test--at-columns 40 (thb-md-render--rewrap-cached-content))
      ;; Wrapping happened, and no continuation cell lost the tint.
      (should (> (count-lines (point-min) (point-max)) 12))
      (should (equal (thb-md-test--quote-bg-holes) nil)))))

(ert-deftest thb-md-render-quoted-code-card-padding-has-bar ()
  (thb-md-test--rendering "> ```\n> code\n> ```\n\nafter\n"
    (should (string-match-p "^▎ \n▎   code\n▎ \n\nafter" (thb-md-test--text)))))

(ert-deftest thb-md-render-rewrap-keeps-nested-bullet-indent-in-quote ()
  (thb-md-test--rendering
      (concat "> - top\n>   - " (mapconcat #'identity (make-list 20 "nested") " ") "\n")
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (thb-md-test--at-columns 40
        (thb-md-render--rewrap-cached-content))
      (let ((text (thb-md-test--text)))
        (should (string-match-p "^▎   • nested" text))
        (should (string-match-p "^▎     nested" text))))))

;;;; Reading features ------------------------------------------------------

(defun thb-md-test--face-has (pos face)
  "Return non-nil if the face at POS is or contains FACE."
  (memq face (ensure-list (get-text-property pos 'face))))

(ert-deftest thb-md-render-alert-renders-callout ()
  (thb-md-test--rendering "> [!WARNING]\n> Careful here.\n> More.\n"
    (let ((text (thb-md-test--text)))
      (should (string-match-p "^▎ Warning\n▎ Careful here\\. More\\.$" text))
      (should-not (string-match-p "!" text)))
    (goto-char (point-min))
    (search-forward "Warning")
    (should (thb-md-test--face-has (1- (point)) 'thb-md-render-alert-warning))
    (search-forward "Careful")
    ;; The bar on body lines takes the alert's colour too.
    (should (thb-md-test--face-has (line-beginning-position)
                                   'thb-md-render-alert-warning))))

(ert-deftest thb-md-render-alert-types-are-case-insensitive ()
  (thb-md-test--rendering "> [!tip]\n> Hint.\n"
    (should (string-match-p "^▎ Tip\n▎ Hint\\.$" (thb-md-test--text)))))

(ert-deftest thb-md-render-non-alerts-stay-literal ()
  (dolist (case '(("> [!NOTE] same line\n" . "^▎ \\[!NOTE\\] same line$")
                  ("> [!nope]\n> x\n" . "^▎ \\[!nope\\] x$")))
    (ert-info ((car case))
      (thb-md-test--rendering (car case)
        (should (string-match-p (cdr case) (thb-md-test--text)))))))

(ert-deftest thb-md-render-reference-links-resolve ()
  (thb-md-test--rendering
      "See [the docs][Ref] and [Ref][] and [ref] and [nope][x] and [y].\n\n[ref]: <https://example.com> \"Title\"\n"
    (let ((text (thb-md-test--text)))
      (should (string-match-p "^See the docs and Ref and ref and \\[nope\\]\\[x\\] and \\[y\\]\\.$" text))
      (should-not (string-match-p "example\\.com\\|Title" text)))
    (goto-char (point-min))
    (dolist (word '("docs" "Ref" "ref"))
      (search-forward word)
      (should (equal (get-text-property (1- (point)) 'thb-md-link)
                     "https://example.com")))))

(defconst thb-md-test--outline
  "# Intro\n\ntext [go](#sub_part-two-1)\n\n## Sub_Part Two!\n\npara\n\n## Sub_Part Two!\n\n### Deep\n\n```\n# not heading\n```\n\n## Último paso\n"
  "Document with duplicate headings, a code comment and non-ASCII text.")

(ert-deftest thb-md-render-headings-get-github-anchors ()
  (thb-md-test--rendering thb-md-test--outline
    (should (equal (mapcar (lambda (h) (nth 2 h)) (thb-md-render--headings))
                   '("intro" "sub_part-two" "sub_part-two-1" "deep" "último-paso")))))

(ert-deftest thb-md-render-heading-navigation ()
  (thb-md-test--rendering thb-md-test--outline
    (cl-flet ((here () (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))))
      (goto-char (point-min))
      (thb-md-render-next-heading)
      (should (equal (here) "Sub_Part Two!"))
      (thb-md-render-next-heading)
      (thb-md-render-next-heading)
      (should (equal (here) "Deep"))
      ;; The `#' comment inside the code block is not a heading.
      (thb-md-render-next-heading)
      (should (equal (here) "Último paso"))
      (should-error (thb-md-render-next-heading) :type 'user-error)
      (thb-md-render-previous-heading)
      (should (equal (here) "Deep"))
      (thb-md-render-up-heading)
      ;; Deep's parent is the second "Sub_Part Two!".
      (should (equal (nth 1 (thb-md-render--heading-at (point))) "sub_part-two-1"))
      (thb-md-render-backward-same-level)
      (should (equal (here) "Sub_Part Two!"))
      (thb-md-render-forward-same-level)
      (thb-md-render-forward-same-level)
      (should (equal (here) "Último paso")))))

(ert-deftest thb-md-render-imenu-lists-heading-paths ()
  (thb-md-test--rendering thb-md-test--outline
    (should (equal (mapcar #'car (funcall imenu-create-index-function))
                   '("Intro" "Intro / Sub_Part Two!" "Intro / Sub_Part Two!"
                     "Intro / Sub_Part Two! / Deep" "Intro / Último paso")))))

(ert-deftest thb-md-render-follow-fragment-link ()
  (thb-md-test--rendering thb-md-test--outline
    (goto-char (point-min))
    (search-forward "go")
    (should (eq (lookup-key (get-text-property (1- (point)) 'keymap) (kbd "RET"))
                #'thb-md-render-follow-link))
    (thb-md-render-follow-link (1- (point)))
    ;; The second "Sub_Part Two!", not the first.
    (should (equal (nth 1 (thb-md-render--heading-at (point))) "sub_part-two-1"))))

(ert-deftest thb-md-render-follow-links-to-files-and-urls ()
  (let* ((dir (make-temp-file "thb-md-links-" t))
         (a (expand-file-name "a.md" dir))
         (b (expand-file-name "b.md" dir))
         (c (expand-file-name "c.txt" dir))
         (thb-md-render-text-scale 0)
         (thb-md-render-body-width nil)
         browsed buffers)
    (with-temp-file a
      (insert "[b](b.md#target) [c](c.txt#L3) [web](https://example.com/x)\n"))
    (with-temp-file b (insert "# Other\n\n## Target\n"))
    (with-temp-file c (insert "one\ntwo\nthree\nfour\n"))
    (unwind-protect
        (save-window-excursion
          (let ((preview-a (thb-md-render-file a)))
            (push preview-a buffers)
            (switch-to-buffer preview-a)
            (cl-letf (((symbol-function 'browse-url)
                       (lambda (url &rest _) (setq browsed url))))
              (goto-char (point-min))
              (search-forward "web")
              (thb-md-render-follow-link (1- (point)))
              (should (equal browsed "https://example.com/x")))
            (goto-char (point-min))
            (thb-md-render-follow-link (point))
            (push (current-buffer) buffers)
            (should (derived-mode-p 'thb-md-render-mode))
            (should (equal thb-md-render--source-file (file-truename b)))
            (should (equal (nth 1 (thb-md-render--heading-at (point))) "target"))
            (xref-go-back)
            (should (eq (current-buffer) preview-a))
            (goto-char (point-min))
            (search-forward "c")
            (thb-md-render-follow-link (1- (point)))
            (push (current-buffer) buffers)
            (should (equal (file-truename buffer-file-name) (file-truename c)))
            (should (= (line-number-at-pos) 3))))
      (mapc (lambda (buf) (when (buffer-live-p buf) (kill-buffer buf))) buffers)
      (delete-directory dir t))))

(ert-deftest thb-md-render-missing-link-target-is-a-user-error ()
  (thb-md-test--rendering "[gone](missing.md) [none](#nowhere)\n"
    (goto-char (point-min))
    (should-error (thb-md-render-follow-link (point)) :type 'user-error)
    (search-forward "none")
    (should-error (thb-md-render-follow-link (1- (point))) :type 'user-error)))

(ert-deftest thb-md-render-next-link-moves-between-links ()
  (thb-md-test--rendering "a [one](#x) b [two](#y)\n"
    (goto-char (point-min))
    (thb-md-render-next-link)
    (should (looking-at "one"))
    (thb-md-render-next-link)
    (should (looking-at "two"))
    (should-error (thb-md-render-next-link) :type 'user-error)
    (thb-md-render-previous-link)
    (should (looking-at "one"))))

;;;; Review regressions ----------------------------------------------------

(ert-deftest thb-md-render-bodyless-alert-pads-in-place ()
  (dolist (case '(("> [!NOTE]\n\nafter\n# Next\n" . "\\`▎ \n▎ Note\n▎ \n\nafter\n")
                  ("- a\n\n  > [!TIP]\n- b\n" . "  ▎ Tip\n  ▎ \n")))
    (ert-info ((car case))
      (thb-md-test--rendering (car case)
        (let ((text (thb-md-test--text)))
          (should (string-match-p (cdr case) text))
          ;; Nothing after the alert carries its bar.
          (should-not (string-match-p "▎[^\n]*\n[[:space:]]*\\'"
                                      (string-trim-right text "[ \n]+"))))))))

(ert-deftest thb-md-render-alert-body-starts-under-label ()
  (dolist (case '(("> [!CAUTION]\n> ```sh\n> rm\n> ```\n" . "^▎ Caution\n▎ \n▎   sh\n")
                  ("> [!TIP]\n> - x\n" . "^▎ Tip\n▎ • x\n")))
    (ert-info ((car case))
      (thb-md-test--rendering (car case)
        (should (string-match-p (cdr case) (thb-md-test--text)))))))

(ert-deftest thb-md-render-link-destinations-are-decoded ()
  (thb-md-test--rendering "[a](<sub dir/f.md>) [y](a&amp;b) [z](foo\\_bar)\n"
    (goto-char (point-min))
    (dolist (case '(("a" . "sub dir/f.md") ("y" . "a&b") ("z" . "foo_bar")))
      (search-forward (car case))
      (should (equal (get-text-property (1- (point)) 'thb-md-link) (cdr case))))))

(ert-deftest thb-md-render-angle-bracket-link-is-followable ()
  (let* ((dir (make-temp-file "thb-md-links-" t))
         (a (expand-file-name "a.md" dir))
         (thb-md-render-text-scale 0)
         (thb-md-render-body-width nil)
         buffers)
    (make-directory (expand-file-name "sub dir" dir))
    (with-temp-file (expand-file-name "sub dir/other file.md" dir) (insert "# Other\n"))
    (with-temp-file a (insert "[o](<sub dir/other file.md>)\n"))
    (unwind-protect
        (save-window-excursion
          (let ((preview (thb-md-render-file a)))
            (push preview buffers)
            (switch-to-buffer preview)
            (thb-md-render-follow-link (point-min))
            (push (current-buffer) buffers)
            (should (equal thb-md-render--source-file
                           (file-truename (expand-file-name "sub dir/other file.md" dir))))))
      (mapc (lambda (b) (when (buffer-live-p b) (kill-buffer b))) buffers)
      (delete-directory dir t))))

(ert-deftest thb-md-render-heading-closing-sequence-is-dropped ()
  (thb-md-test--rendering "# Title #\n\n## Sub ##   \n\n### Keep # inside\n\n#### Escaped \\#\n"
    (should (equal (mapcar (lambda (h) (list (nth 2 h) (nth 3 h))) (thb-md-render--headings))
                   '(("title" "Title") ("sub" "Sub") ("keep--inside" "Keep # inside")
                     ("escaped-" "Escaped #"))))
    (should-not (string-match-p "^Title #\\|^Sub #" (thb-md-test--text))))
  ;; A setext heading has no closing sequence: its trailing # is text.
  (thb-md-test--rendering "Setext #\n===\n"
    (should (string-match-p "^Setext #$" (thb-md-test--text)))))

(ert-deftest thb-md-render-heading-titles-exclude-images ()
  (thb-md-test--rendering "## Logo ![alt](x.png)\n"
    (should (equal (mapcar (lambda (h) (list (nth 2 h) (nth 3 h))) (thb-md-render--headings))
                   '(("logo" "Logo"))))
    ;; The image placeholder still renders in the heading.
    (should (string-match-p "^Logo 🖼 alt x\\.png$" (thb-md-test--text)))))

;;;; Fitted tables ---------------------------------------------------------

(defconst thb-md-test--wide-table
  (concat "| Name | Description | N |\n| --- | --- | ---: |\n"
          "| alpha | " (mapconcat #'identity (make-list 12 "words") " ") " | 1 |\n"
          "| beta | short | 22 |\n")
  "A table wider than 40 columns with one long cell.")

(defun thb-md-test--table-lines ()
  "Return the non-blank lines of the preview, as strings."
  (seq-remove #'string-empty-p (split-string (thb-md-test--text) "\n")))

(ert-deftest thb-md-render-table-fits-narrow-window ()
  (thb-md-test--rendering thb-md-test--wide-table
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (thb-md-test--at-columns 40 (thb-md-render--rewrap-cached-content))
      (let ((lines (thb-md-test--table-lines)))
        (dolist (line lines)
          (should (<= (string-width line) 39)))
        ;; Every content line has the same column separators.
        (let ((bars (mapcar (lambda (l) (cl-count ?│ l))
                            (seq-remove (lambda (l) (string-match-p "\\`[─┼]+\\'" l))
                                        lines))))
          (should (cl-every (lambda (n) (= n 2)) bars)))
        ;; All words of the long cell survive, wrapped onto several lines.
        (should (= (length (thb-md-test--tokens-of "words" (thb-md-test--text))) 12))
        (should (> (cl-count-if (lambda (l) (string-match-p "words" l)) lines) 1))
        ;; Right-aligned numbers stay right-aligned.
        (should (cl-some (lambda (l) (string-match-p "│ +22\\'" l)) lines))))))

(defun thb-md-test--tokens-of (word text)
  "Return every occurrence of WORD in TEXT."
  (let ((pos 0) found)
    (while (string-match (regexp-quote word) text pos)
      (push (match-string 0 text) found)
      (setq pos (match-end 0)))
    found))

(ert-deftest thb-md-render-table-refits-on-reflow ()
  (thb-md-test--rendering thb-md-test--wide-table
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (let ((wide (thb-md-test--at-columns 200
                    (thb-md-render--rewrap-cached-content) (thb-md-test--text))))
        ;; Wide enough: natural layout, no separators, one line per row.
        (should-not (string-match-p "│" wide))
        (should (string-match-p "^alpha  words" wide))
        (thb-md-test--at-columns 40 (thb-md-render--rewrap-cached-content))
        (should (string-match-p "│" (thb-md-test--text)))
        (thb-md-test--at-columns 200 (thb-md-render--rewrap-cached-content))
        (should (equal (thb-md-test--text) wide))))))

(ert-deftest thb-md-render-fitted-table-keeps-container-prefix ()
  (thb-md-test--rendering (concat "> " (string-replace "\n" "\n> " (string-trim-right thb-md-test--wide-table)) "\n")
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (thb-md-test--at-columns 40 (thb-md-render--rewrap-cached-content))
      (dolist (line (thb-md-test--table-lines))
        (should (string-prefix-p "▎ " line))
        (should (<= (string-width line) 39))))))

(ert-deftest thb-md-render-fitted-table-wraps-wide-characters ()
  (thb-md-test--rendering
      (concat "| a | b |\n| --- | --- |\n| x | "
              (mapconcat #'identity (make-list 10 "中文字符") " ") " |\n")
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (thb-md-test--at-columns 30 (thb-md-render--rewrap-cached-content))
      (dolist (line (thb-md-test--table-lines))
        (should (<= (string-width line) 29))))))

;;;; Unchanged features ---------------------------------------------------

(ert-deftest thb-md-render-setext-heading ()
  (thb-md-test--rendering "Title *x*\n=====\n\nSub\n---\n"
    (should (string-match-p "^Title x$" (thb-md-test--text)))
    (goto-char (point-min))
    (search-forward "Title")
    (should (eq (plist-get (get-text-property (1- (point)) 'face) :inherit)
                'thb-md-render-h1))
    (search-forward "Sub")
    (should (eq (plist-get (get-text-property (1- (point)) 'face) :inherit)
                'thb-md-render-h2))))

(ert-deftest thb-md-render-image-alt-and-source ()
  (thb-md-test--rendering "![*alt*](pic.png)\n"
    (should (string-match-p "^🖼 alt pic\\.png$" (thb-md-test--text)))
    (goto-char (point-min))
    (search-forward "alt")
    (let ((face (get-text-property (1- (point)) 'face)))
      (should (memq 'thb-md-render-emphasis face))
      (should (memq 'thb-md-render-link-text face)))))

(ert-deftest thb-md-render-fontifies-fenced-code ()
  (thb-md-test--rendering "```elisp\n(defun f () \"s\")\n```\n"
    (goto-char (point-min))
    (search-forward "defun")
    (let ((face (get-text-property (1- (point)) 'face)))
      (should (memq 'font-lock-keyword-face (ensure-list face)))
      (should (memq 'thb-md-render-code-block (ensure-list face))))))

;;;; Paths batch rendering does not reach ----------------------------------

(ert-deftest thb-md-render-graphical-wrap-caches-space-widths ()
  (let ((space-calls 0))
    (with-temp-buffer
      (insert (propertize (mapconcat #'identity (make-list 50 "word") " ")
                          'face 'thb-md-render-body))
      (cl-letf (((symbol-function 'thb-md-render--wrapping-window) #'selected-window)
                ((symbol-function 'display-graphic-p) (lambda (&optional _) t))
                ((symbol-function 'thb-md-render--prose-budget-px) (lambda () 205))
                ((symbol-function 'string-pixel-width)
                 (lambda (s &rest _)
                   (when (equal s " ") (cl-incf space-calls))
                   (* 10 (length s)))))
        (thb-md-render--rewrap-prose))
      (should (= space-calls 1))
      (goto-char (point-min))
      (while (not (eobp))
        (should (<= (* 10 (- (line-end-position) (line-beginning-position))) 205))
        (forward-line 1))
      (should (= (count-lines (point-min) (point-max)) 13)))))

(ert-deftest thb-md-render-toggle-wraps-once ()
  (let* ((path (make-temp-file "thb-md-test-" nil ".md"
                               (concat (mapconcat (lambda (_) "prose") (number-sequence 1 60) " ")
                                       "\n")))
         (thb-md-render-text-scale 0)
         (thb-md-render-body-width nil)
         (source (find-file-noselect path))
         (wraps 0)
         (count (lambda (&rest _) (cl-incf wraps)))
         preview)
    (advice-add 'thb-md-render--rewrap-prose :before count)
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (markdown-ts-mode)
          (thb-md-render-toggle)
          (setq preview (current-buffer))
          (should (derived-mode-p 'thb-md-render-mode))
          ;; What the first window-configuration change would do.
          (thb-md-render--reflow-now)
          (should (= wraps 1)))
      (advice-remove 'thb-md-render--rewrap-prose count)
      (when (buffer-live-p preview) (kill-buffer preview))
      (kill-buffer source)
      (delete-file path))))

(ert-deftest thb-md-render-toggle-error-returns-to-source ()
  (let* ((path (make-temp-file "thb-md-test-" nil ".md" "text\n"))
         (source (find-file-noselect path)))
    (unwind-protect
        (save-window-excursion
          (switch-to-buffer source)
          (markdown-ts-mode)
          (cl-letf (((symbol-function 'thb-md-render-file)
                     (lambda (&rest _) (error "Render failed"))))
            (should-error (thb-md-render-toggle)))
          (should (eq (window-buffer (selected-window)) source)))
      (let ((preview (get-buffer (thb-md-render--preview-buffer-name
                                  (thb-md-render--canonical-source-file path)))))
        (when preview (kill-buffer preview)))
      (kill-buffer source)
      (delete-file path))))

(ert-deftest thb-md-render-reflow-keeps-olivetti-margins ()
  (let ((dir (car (file-expand-wildcards
                   (expand-file-name "~/.config/thbemacs/elpa/olivetti-*")))))
    (skip-unless (and dir (progn (add-to-list 'load-path dir)
                                 (require 'olivetti nil t))))
    (let ((path (make-temp-file "thb-md-test-" nil ".md"
                                (concat (mapconcat (lambda (_) "prose")
                                                   (number-sequence 1 60) " ")
                                        "\n")))
          (thb-md-render-text-scale 0)
          (thb-md-render-body-width 40)
          preview)
      (unwind-protect
          (save-window-excursion
            (delete-other-windows)
            (setq preview (thb-md-render-file path))
            (set-window-buffer (selected-window) preview)
            (with-current-buffer preview
              (should olivetti-mode)
              (thb-md-render--rewrap-cached-content)
              ;; A reflow must leave the reading column, not a full-width window.
              (should (car (window-margins)))
              (should (< (window-body-width) (window-total-width)))
              (goto-char (point-min))
              (should (<= (string-width (buffer-substring (point) (line-end-position)))
                          (window-body-width)))))
        (when (buffer-live-p preview) (kill-buffer preview))
        (delete-file path)))))

(ert-deftest thb-md-render-restore-view-keeps-point-and-start ()
  (with-temp-buffer
    (dotimes (i 200) (insert (format "line %d\n" i)))
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (let ((win (selected-window)))
        (set-window-start win (progn (goto-char (point-min)) (forward-line 50) (point)))
        (set-window-point win (progn (forward-line 10) (point)))
        (let ((view (thb-md-render--capture-view win))
              (text (buffer-string)))
          (erase-buffer)
          (insert text)
          (goto-char (point-min))
          (thb-md-render--restore-view view win)
          (should (= (line-number-at-pos (window-start win)) 51))
          (should (= (line-number-at-pos (window-point win)) 61)))))))

;;; thb-markdown-render-reading-test.el ends here
