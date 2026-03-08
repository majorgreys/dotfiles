;;; test-md-table.el --- ERT tests for markdown table renderer -*- lexical-binding: t; -*-

;; Run: emacs --batch -l test-md-table.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)

;;; ============================================================
;;; Functions under test (extracted from init.el agent-shell config)
;;; ============================================================

(defun thb/md-table-render-inline (str)
  "Render markdown inline formatting in STR. Return propertized string."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (re-search-forward "\\*\\*\\([^*]+\\)\\*\\*" nil t)
      (replace-match (propertize (match-string 1) 'face 'bold)))
    (goto-char (point-min))
    (while (re-search-forward "`\\([^`]+\\)`" nil t)
      (replace-match (propertize (match-string 1) 'face 'fixed-pitch)))
    (buffer-string)))

(defun thb/md-table-wrap-string (str width)
  "Wrap STR at word boundaries to fit WIDTH. Return list of lines."
  (if (<= (length str) width)
      (list str)
    (with-temp-buffer
      (insert str)
      (let ((fill-column width))
        (fill-region (point-min) (point-max)))
      (split-string (buffer-string) "\n"))))

(defun thb/md-table-parse (text)
  "Parse pipe-delimited TEXT into (HEADER . DATA-ROWS)."
  (let (header rows)
    (dolist (line (split-string text "\n" t))
      (unless (string-match-p "^[ \t]*|[-+|: ]+|?[ \t]*$" line)
        (when (string-match "^[ \t]*|\\(.*\\)|[ \t]*$" line)
          (let ((cells (mapcar (lambda (c)
                                 (thb/md-table-render-inline (string-trim c)))
                               (split-string (match-string 1 line) "|"))))
            (if header (push cells rows) (setq header cells))))))
    (cons header (nreverse rows))))

(defun thb/md-table-col-widths (header rows usable)
  "Compute column widths for HEADER and ROWS to fit USABLE chars."
  (let* ((ncols (length header))
         (max-w (make-list ncols 0)))
    (dolist (row (cons header rows))
      (dotimes (i (min ncols (length row)))
        (setf (nth i max-w) (max (nth i max-w) (length (nth i row))))))
    (let ((total (apply #'+ max-w)))
      (if (<= total usable)
          max-w
        (let ((threshold 12) (short-total 0) long-idxs)
          (dotimes (i ncols)
            (if (<= (nth i max-w) threshold)
                (cl-incf short-total (nth i max-w))
              (push i long-idxs)))
          (let* ((remaining (- usable short-total))
                 (long-total (apply #'+ (mapcar (lambda (i) (nth i max-w)) long-idxs)))
                 (result (copy-sequence max-w)))
            (dolist (i long-idxs)
              (setf (nth i result)
                    (max 8 (floor (* remaining (/ (float (nth i max-w)) long-total))))))
            result))))))

(defun thb/md-table-render-row (cells col-widths)
  "Render CELLS with COL-WIDTHS, wrapping cell text. Return string."
  (let* ((ncols (length col-widths))
         (wrapped (cl-loop for i below ncols
                           collect (thb/md-table-wrap-string
                                    (or (nth i cells) "") (nth i col-widths))))
         (nlines (apply #'max 1 (mapcar #'length wrapped)))
         lines)
    (dotimes (li nlines)
      (push (concat "│ "
                    (mapconcat
                     (lambda (i)
                       (let ((text (or (nth li (nth i wrapped)) ""))
                             (w (nth i col-widths)))
                         (concat text (make-string (max 0 (- w (length text))) ?\s))))
                     (number-sequence 0 (1- ncols)) " │ ")
                    " │")
            lines))
    (string-join (nreverse lines) "\n")))

(defun thb/md-table-format (table-text win-width)
  "Format TABLE-TEXT with box-drawing to fit WIN-WIDTH."
  (let* ((parsed (thb/md-table-parse table-text))
         (header (car parsed))
         (rows (cdr parsed))
         (ncols (length header))
         (overhead (+ (* ncols 3) 1))
         (col-widths (thb/md-table-col-widths header rows (- win-width overhead)))
         (rule (lambda (l m r)
                 (concat l (mapconcat (lambda (w) (make-string (+ w 2) ?─))
                                      col-widths m) r))))
    (concat (funcall rule "┌" "┬" "┐") "\n"
            (thb/md-table-render-row header col-widths) "\n"
            (funcall rule "├" "┼" "┤") "\n"
            (mapconcat (lambda (row) (thb/md-table-render-row row col-widths))
                       rows "\n") "\n"
            (funcall rule "└" "┴" "┘"))))


;;; ============================================================
;;; thb/md-table-render-inline
;;; ============================================================

(ert-deftest md-table-render-inline/plain-text ()
  "Plain text passes through unchanged."
  (should (equal "hello world" (thb/md-table-render-inline "hello world"))))

(ert-deftest md-table-render-inline/bold ()
  "**bold** is rendered with bold face."
  (let ((result (thb/md-table-render-inline "**bold**")))
    (should (equal "bold" result))
    (should (eq 'bold (get-text-property 0 'face result)))))

(ert-deftest md-table-render-inline/inline-code ()
  "`code` is rendered with fixed-pitch face."
  (let ((result (thb/md-table-render-inline "`code`")))
    (should (equal "code" result))
    (should (eq 'fixed-pitch (get-text-property 0 'face result)))))

(ert-deftest md-table-render-inline/mixed ()
  "Mixed bold and code in one string."
  (let ((result (thb/md-table-render-inline "**bold** and `code`")))
    (should (string-match-p "bold" result))
    (should (string-match-p "code" result))
    ;; Markup characters should be stripped
    (should-not (string-match-p "\\*\\*" result))
    (should-not (string-match-p "`" result))))

(ert-deftest md-table-render-inline/no-markup ()
  "String with no markup is returned as-is."
  (should (equal "no markup here" (thb/md-table-render-inline "no markup here"))))

(ert-deftest md-table-render-inline/empty ()
  "Empty string returns empty."
  (should (equal "" (thb/md-table-render-inline ""))))


;;; ============================================================
;;; thb/md-table-wrap-string
;;; ============================================================

(ert-deftest md-table-wrap/short-string ()
  "String shorter than width is returned as single-element list."
  (should (equal '("hello") (thb/md-table-wrap-string "hello" 20))))

(ert-deftest md-table-wrap/exact-width ()
  "String exactly at width is not wrapped."
  (should (equal '("abcde") (thb/md-table-wrap-string "abcde" 5))))

(ert-deftest md-table-wrap/long-string ()
  "String longer than width is wrapped into multiple lines."
  (let ((result (thb/md-table-wrap-string "hello world foo bar" 10)))
    (should (> (length result) 1))
    (dolist (line result)
      (should (<= (length line) 12)))))  ; fill allows slight overshoot

(ert-deftest md-table-wrap/empty ()
  "Empty string returns single empty element."
  (should (equal '("") (thb/md-table-wrap-string "" 10))))

(ert-deftest md-table-wrap/single-long-word ()
  "A single word longer than width still returns (may not break mid-word)."
  (let ((result (thb/md-table-wrap-string "superlongword" 5)))
    (should (>= (length result) 1))))


;;; ============================================================
;;; thb/md-table-parse
;;; ============================================================

(ert-deftest md-table-parse/basic ()
  "Parse a basic 2-column table with header and separator."
  (let* ((text "| Name | Age |\n|------|-----|\n| Alice | 30 |\n| Bob | 25 |")
         (parsed (thb/md-table-parse text))
         (header (car parsed))
         (rows (cdr parsed)))
    (should (= 2 (length header)))
    (should (equal "Name" (substring-no-properties (nth 0 header))))
    (should (equal "Age" (substring-no-properties (nth 1 header))))
    (should (= 2 (length rows)))
    (should (equal "Alice" (substring-no-properties (nth 0 (nth 0 rows)))))
    (should (equal "30" (substring-no-properties (nth 1 (nth 0 rows)))))))

(ert-deftest md-table-parse/separator-skipped ()
  "Separator line (dashes) is not included as a data row."
  (let* ((text "| A | B |\n|---|---|\n| 1 | 2 |")
         (parsed (thb/md-table-parse text)))
    (should (= 1 (length (cdr parsed))))))

(ert-deftest md-table-parse/colon-alignment-separator ()
  "Separator with colons (alignment) is still skipped."
  (let* ((text "| A | B |\n|:--|--:|\n| 1 | 2 |")
         (parsed (thb/md-table-parse text)))
    (should (= 1 (length (cdr parsed))))))

(ert-deftest md-table-parse/empty-cells ()
  "Table with empty cells parses correctly."
  (let* ((text "| A | B | C |\n|---|---|---|\n|  | x |  |")
         (parsed (thb/md-table-parse text))
         (row (nth 0 (cdr parsed))))
    (should (= 3 (length row)))
    (should (equal "" (substring-no-properties (nth 0 row))))
    (should (equal "x" (substring-no-properties (nth 1 row))))))

(ert-deftest md-table-parse/single-row ()
  "Table with header and one data row."
  (let* ((text "| H1 | H2 |\n|----|----|  \n| v1 | v2 |")
         (parsed (thb/md-table-parse text)))
    (should (= 2 (length (car parsed))))
    (should (= 1 (length (cdr parsed))))))

(ert-deftest md-table-parse/no-data-rows ()
  "Table with only header and separator returns empty rows."
  (let* ((text "| H1 | H2 |\n|----|----|\n")
         (parsed (thb/md-table-parse text)))
    (should (car parsed))
    (should (null (cdr parsed)))))

(ert-deftest md-table-parse/leading-whitespace ()
  "Table with leading whitespace on lines."
  (let* ((text "  | A | B |\n  |---|---|\n  | 1 | 2 |")
         (parsed (thb/md-table-parse text)))
    (should (= 2 (length (car parsed))))
    (should (= 1 (length (cdr parsed))))))


;;; ============================================================
;;; thb/md-table-col-widths
;;; ============================================================

(ert-deftest md-table-col-widths/fits ()
  "When content fits, return natural widths."
  (let ((widths (thb/md-table-col-widths '("Name" "Age") '(("Alice" "30")) 40)))
    (should (= 5 (nth 0 widths)))   ; "Alice" = 5
    (should (= 3 (nth 1 widths))))) ; "Age" = 3

(ert-deftest md-table-col-widths/shrinks-long ()
  "When content exceeds usable, long columns are shrunk."
  (let* ((long-header (make-string 30 ?x))
         (widths (thb/md-table-col-widths
                  (list long-header "ID")
                  (list (list (make-string 30 ?y) "1"))
                  20)))
    ;; Short column (ID=2, under threshold 12) keeps its width
    (should (= 2 (nth 1 widths)))
    ;; Long column is shrunk but at least 8
    (should (>= (nth 0 widths) 8))
    (should (< (nth 0 widths) 30))))

(ert-deftest md-table-col-widths/minimum-8 ()
  "Long columns are never shrunk below 8."
  (let* ((widths (thb/md-table-col-widths
                  (list (make-string 50 ?a) (make-string 50 ?b))
                  nil
                  20)))
    (should (>= (nth 0 widths) 8))
    (should (>= (nth 1 widths) 8))))


;;; ============================================================
;;; thb/md-table-render-row
;;; ============================================================

(ert-deftest md-table-render-row/basic ()
  "Render a simple row with padding."
  (let ((result (thb/md-table-render-row '("hi" "there") '(5 7))))
    (should (string-match-p "│" result))
    (should (string-match-p "hi" result))
    (should (string-match-p "there" result))))

(ert-deftest md-table-render-row/padding ()
  "Cells are padded to column width."
  (let ((result (thb/md-table-render-row '("a" "b") '(5 5))))
    ;; "a" should be followed by 4 spaces to fill width 5
    (should (string-match-p "a    " result))
    (should (string-match-p "b    " result))))

(ert-deftest md-table-render-row/empty-cells ()
  "Missing cells are treated as empty strings."
  (let ((result (thb/md-table-render-row '("x") '(5 5))))
    ;; Should not error; second cell defaults to ""
    (should (stringp result))
    (should (string-match-p "│" result))))

(ert-deftest md-table-render-row/wrapping ()
  "Long cell content wraps into multiple lines."
  (let ((result (thb/md-table-render-row '("hello world foo" "x") '(8 3))))
    ;; Should have multiple lines (newlines in output)
    (should (string-match-p "\n" result))))


;;; ============================================================
;;; thb/md-table-format (integration)
;;; ============================================================

(ert-deftest md-table-format/basic-table ()
  "Format a complete table with box drawing."
  (let* ((text "| Name | Age |\n|------|-----|\n| Alice | 30 |")
         (result (thb/md-table-format text 40)))
    ;; Has box-drawing corners
    (should (string-match-p "┌" result))
    (should (string-match-p "┐" result))
    (should (string-match-p "└" result))
    (should (string-match-p "┘" result))
    ;; Has header separator
    (should (string-match-p "├" result))
    (should (string-match-p "┤" result))
    ;; Contains data
    (should (string-match-p "Alice" result))
    (should (string-match-p "30" result))))

(ert-deftest md-table-format/narrow-width ()
  "Table renders without error at narrow width."
  (let* ((text "| Name | Description |\n|------|-------------|\n| Alice | A very long description that should wrap |")
         (result (thb/md-table-format text 30)))
    (should (stringp result))
    (should (string-match-p "Alice" result))))

(ert-deftest md-table-format/three-columns ()
  "Three-column table renders correctly."
  (let* ((text "| A | B | C |\n|---|---|---|\n| 1 | 2 | 3 |")
         (result (thb/md-table-format text 40)))
    ;; Should have column separators (┬ in top rule for 3 cols = 2 ┬)
    (should (= 2 (cl-count ?┬ result)))))

(ert-deftest md-table-format/multiple-rows ()
  "Multiple data rows all appear in output."
  (let* ((text "| X | Y |\n|---|---|\n| a | 1 |\n| b | 2 |\n| c | 3 |")
         (result (thb/md-table-format text 40)))
    (should (string-match-p "a" result))
    (should (string-match-p "b" result))
    (should (string-match-p "c" result))))


;;; test-md-table.el ends here
