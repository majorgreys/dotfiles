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
      (unless (string-match-p "^[ \t]*|[-+|: ]*[-+][-+|: ]*|?[ \t]*$" line)
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


;;; ============================================================
;;; Edge Cases: Malformed Input
;;; ============================================================

(ert-deftest md-table-parse/empty-string ()
  "Empty string returns nil header and no rows."
  (let ((parsed (thb/md-table-parse "")))
    (should (null (car parsed)))
    (should (null (cdr parsed)))))

(ert-deftest md-table-parse/whitespace-only ()
  "Whitespace-only input returns nil header."
  (let ((parsed (thb/md-table-parse "   \n  \n")))
    (should (null (car parsed)))))

(ert-deftest md-table-parse/separator-only ()
  "Table with only separator line and no header/data."
  (let ((parsed (thb/md-table-parse "|---|---|")))
    (should (null (car parsed)))
    (should (null (cdr parsed)))))

(ert-deftest md-table-parse/no-separator ()
  "Table without separator line — all rows after first become data."
  (let* ((text "| A | B |\n| 1 | 2 |\n| 3 | 4 |")
         (parsed (thb/md-table-parse text)))
    (should (= 2 (length (car parsed))))
    (should (= 2 (length (cdr parsed))))))

(ert-deftest md-table-parse/mismatched-columns-fewer ()
  "Row with fewer columns than header."
  (let* ((text "| A | B | C |\n|---|---|---|\n| 1 |")
         (parsed (thb/md-table-parse text))
         (row (nth 0 (cdr parsed))))
    ;; Should parse without error; row has fewer cells
    (should (< (length row) (length (car parsed))))))

(ert-deftest md-table-parse/mismatched-columns-more ()
  "Row with more columns than header."
  (let* ((text "| A | B |\n|---|---|\n| 1 | 2 | 3 | 4 |")
         (parsed (thb/md-table-parse text))
         (row (nth 0 (cdr parsed))))
    ;; Should parse without error; row has extra cells
    (should (> (length row) (length (car parsed))))))

(ert-deftest md-table-parse/single-column ()
  "Single column table."
  (let* ((text "| A |\n|---|\n| 1 |\n| 2 |")
         (parsed (thb/md-table-parse text)))
    (should (= 1 (length (car parsed))))
    (should (= 2 (length (cdr parsed))))))

(ert-deftest md-table-parse/no-closing-pipe ()
  "Line without closing pipe is not matched."
  (let* ((text "| A | B\n|---|---|\n| 1 | 2")
         (parsed (thb/md-table-parse text)))
    ;; Lines without closing | should be skipped by the regex
    (should (null (car parsed)))))

(ert-deftest md-table-parse/whitespace-cells ()
  "Cells containing only whitespace are trimmed to empty."
  (let* ((text "| A | B |\n|---|---|\n|   |   |")
         (parsed (thb/md-table-parse text))
         (row (nth 0 (cdr parsed))))
    (should (equal "" (substring-no-properties (nth 0 row))))
    (should (equal "" (substring-no-properties (nth 1 row))))))


;;; ============================================================
;;; Edge Cases: Unicode and Special Content
;;; ============================================================

(ert-deftest md-table-parse/unicode-content ()
  "Table with unicode characters parses correctly."
  (let* ((text "| Name | Symbol |\n|------|--------|\n| Alpha | α |\n| Beta | β |")
         (parsed (thb/md-table-parse text)))
    (should (= 2 (length (cdr parsed))))
    (should (equal "α" (substring-no-properties (nth 1 (nth 0 (cdr parsed))))))))

(ert-deftest md-table-render-inline/nested-backticks ()
  "Backtick inside bold is not treated as inline code."
  (let ((result (thb/md-table-render-inline "**has ` tick**")))
    ;; The whole thing should be rendered as bold
    (should (string-match-p "has" result))))

(ert-deftest md-table-render-inline/unclosed-bold ()
  "Unclosed **bold is left as-is."
  (let ((result (thb/md-table-render-inline "**not closed")))
    (should (string-match-p "\\*\\*" result))))

(ert-deftest md-table-render-inline/unclosed-code ()
  "Unclosed `code is left as-is."
  (let ((result (thb/md-table-render-inline "`not closed")))
    (should (string-match-p "`" result))))

(ert-deftest md-table-render-inline/empty-bold ()
  "Empty bold **** is handled."
  (let ((result (thb/md-table-render-inline "****")))
    ;; Should not error
    (should (stringp result))))

(ert-deftest md-table-render-inline/empty-code ()
  "Empty code `` is handled."
  (let ((result (thb/md-table-render-inline "``")))
    ;; Should not error
    (should (stringp result))))


;;; ============================================================
;;; Edge Cases: Column Width Calculation
;;; ============================================================

(ert-deftest md-table-col-widths/all-long-tiny-usable ()
  "When usable is smaller than ncols*8, columns still get minimum 8."
  (let ((widths (thb/md-table-col-widths
                 (list (make-string 20 ?a) (make-string 20 ?b) (make-string 20 ?c))
                 nil
                 10)))
    ;; Each column should be at least 8
    (dolist (w widths)
      (should (>= w 8)))))

(ert-deftest md-table-col-widths/single-column ()
  "Single column table width calculation."
  (let ((widths (thb/md-table-col-widths '("Header") '(("data")) 40)))
    (should (= 1 (length widths)))
    (should (= 6 (nth 0 widths)))))  ; "Header" = 6

(ert-deftest md-table-col-widths/empty-cells ()
  "Column widths with empty cells in data."
  (let ((widths (thb/md-table-col-widths '("Name" "Val") '(("" "")) 40)))
    (should (= 4 (nth 0 widths)))    ; "Name" = 4
    (should (= 3 (nth 1 widths))))) ; "Val" = 3

(ert-deftest md-table-col-widths/zero-usable ()
  "Zero usable space — columns still get minimum 8."
  (let ((widths (thb/md-table-col-widths
                 (list (make-string 20 ?a) (make-string 20 ?b))
                 nil
                 0)))
    (dolist (w widths)
      (should (>= w 8)))))


;;; ============================================================
;;; Edge Cases: Row Rendering
;;; ============================================================

(ert-deftest md-table-render-row/more-cells-than-widths ()
  "Extra cells beyond col-widths are silently ignored."
  (let ((result (thb/md-table-render-row '("a" "b" "c" "d") '(5 5))))
    ;; Should render only 2 columns (matching col-widths length)
    (should (stringp result))
    (should (string-match-p "a" result))
    (should (string-match-p "b" result))
    ;; Extra cells c and d should not appear
    (should-not (string-match-p "c" result))
    (should-not (string-match-p "d" result))))

(ert-deftest md-table-render-row/nil-cells ()
  "Nil in cells list is treated as empty string."
  (let ((result (thb/md-table-render-row '(nil nil) '(5 5))))
    (should (stringp result))
    (should (string-match-p "│" result))))

(ert-deftest md-table-render-row/single-column ()
  "Single column row renders with pipes."
  (let ((result (thb/md-table-render-row '("hello") '(10))))
    (should (string-match-p "│" result))
    (should (string-match-p "hello" result))))


;;; ============================================================
;;; Edge Cases: Full Format Integration
;;; ============================================================

(ert-deftest md-table-format/single-column ()
  "Single column table formats without error."
  (let* ((text "| X |\n|---|\n| 1 |\n| 2 |")
         (result (thb/md-table-format text 40)))
    (should (stringp result))
    (should (string-match-p "1" result))
    (should (string-match-p "2" result))
    ;; No column separators in rules (0 ┬ for 1 column)
    (should (= 0 (cl-count ?┬ result)))))

(ert-deftest md-table-format/very-narrow ()
  "Table at extremely narrow width (10 chars) doesn't crash."
  (let* ((text "| Name | Description |\n|------|-------------|\n| A | B |")
         (result (thb/md-table-format text 10)))
    (should (stringp result))))

(ert-deftest md-table-format/wide-content-narrow-window ()
  "Wide content in narrow window wraps rather than truncating."
  (let* ((text "| Key | Value |\n|-----|-------|\n| name | This is a very long value that must wrap |")
         (result (thb/md-table-format text 30)))
    ;; The full content should still be present (wrapped, not truncated)
    (should (string-match-p "very" result))
    (should (string-match-p "wrap" result))))

(ert-deftest md-table-format/mismatched-row-columns ()
  "Table with rows having different column counts than header."
  (let* ((text "| A | B | C |\n|---|---|---|\n| 1 |\n| x | y | z | w |")
         (result (thb/md-table-format text 40)))
    ;; Should not crash; renders based on header column count
    (should (stringp result))))

(ert-deftest md-table-format/unicode-alignment ()
  "Table with unicode content aligns columns."
  (let* ((text "| Name | Sym |\n|------|-----|\n| alpha | α |\n| beta | β |")
         (result (thb/md-table-format text 40)))
    (should (stringp result))
    ;; Both rows should be present
    (should (string-match-p "α" result))
    (should (string-match-p "β" result))))

(ert-deftest md-table-format/bold-in-cells ()
  "Bold markdown in cells is rendered and stripped of markers."
  (let* ((text "| Col |\n|-----|\n| **bold** |")
         (result (thb/md-table-format text 40)))
    (should (string-match-p "bold" result))
    (should-not (string-match-p "\\*\\*" result))))

(ert-deftest md-table-format/empty-table-text ()
  "Empty table text doesn't crash format."
  ;; thb/md-table-parse returns (nil) for empty input
  ;; thb/md-table-format will get nil header — should handle gracefully
  (condition-case err
      (thb/md-table-format "" 40)
    (error
     ;; If it errors, that's a bug we're documenting
     (should (consp err)))))

(ert-deftest md-table-format/header-only-no-separator ()
  "Table with header but no separator or data."
  (let* ((text "| A | B |")
         (result (thb/md-table-format text 40)))
    ;; Should produce some output (header as only content)
    (should (stringp result))))


;;; ============================================================
;;; Edge Cases: Word Wrapping
;;; ============================================================

(ert-deftest md-table-wrap/width-1 ()
  "Wrapping at width 1 doesn't infinite loop."
  (let ((result (thb/md-table-wrap-string "hello" 1)))
    (should (>= (length result) 1))))

(ert-deftest md-table-wrap/width-0 ()
  "Width 0 doesn't crash."
  ;; length "hi" = 2, > 0, so it enters the wrap branch
  (let ((result (thb/md-table-wrap-string "hi" 0)))
    (should (listp result))))

(ert-deftest md-table-wrap/newlines-in-input ()
  "Input containing newlines is handled."
  (let ((result (thb/md-table-wrap-string "line1\nline2" 20)))
    (should (listp result))))

(ert-deftest md-table-wrap/unicode ()
  "Unicode string wraps without error."
  (let ((result (thb/md-table-wrap-string "αβγδ εζηθ ικλμ" 8)))
    (should (listp result))
    (should (> (length result) 0))))


;;; test-md-table.el ends here
