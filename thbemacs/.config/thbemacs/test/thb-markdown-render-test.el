;;; thb-markdown-render-test.el --- Tests for thb-markdown-render  -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(load (expand-file-name "../lisp/thb-markdown-render.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defmacro thb-md-render-test--with-fake-nodes (&rest body)
  "Run BODY with plist-backed tree-sitter nodes."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'treesit-node-type)
              (lambda (node) (plist-get node :type)))
             ((symbol-function 'treesit-node-children)
              (lambda (node &optional _named)
                (plist-get node :children)))
             ((symbol-function 'treesit-node-child-count)
              (lambda (node &optional _named)
                (length (plist-get node :children))))
             ((symbol-function 'treesit-node-child)
              (lambda (node index &optional _named)
                (nth index (plist-get node :children))))
             ((symbol-function 'treesit-node-start)
              (lambda (node) (plist-get node :start)))
             ((symbol-function 'treesit-node-end)
              (lambda (node) (plist-get node :end)))
             ((symbol-function 'thb-md-render--node-text)
              (lambda (node) (plist-get node :text))))
     ,@body))

(ert-deftest thb-md-render-ensure-blank-line-is-local-and-correct ()
  (dolist (case '(("" . "")
                  ("text" . "text\n\n")
                  ("text\n" . "text\n\n")
                  ("text\n\n" . "text\n\n")
                  ("text\n\n\n" . "text\n\n\n")))
    (with-temp-buffer
      (insert (car case))
      (cl-letf (((symbol-function 'looking-back)
                 (lambda (&rest _) (ert-fail "looking-back must not be used"))))
        (thb-md-render--ensure-blank-line))
      (should (equal (buffer-string) (cdr case))))))

(ert-deftest thb-md-render-setup-applies-inline-ranges-before-use ()
  (let (created update-snapshot)
    (cl-letf (((symbol-function 'treesit-parser-create)
               (lambda (language)
                 (push language created)
                 language))
              ((symbol-function 'treesit-range-rules)
               (lambda (&rest arguments) (cons 'ranges arguments)))
              ((symbol-function 'treesit-update-ranges)
               (lambda (&rest _)
                 (setq update-snapshot
                       (list treesit-primary-parser
                             treesit-range-settings)))))
      (with-temp-buffer
        (should (equal (thb-md-render--setup-parsers)
                       '(markdown . markdown-inline)))
        (should (equal (nreverse created)
                       '(markdown markdown-inline)))
        (should (eq treesit-primary-parser 'markdown))
        (should (equal (car treesit-range-settings) 'ranges))
        (should (equal update-snapshot
                       (list 'markdown treesit-range-settings)))
        (should (member '((inline) @capture
                          (pipe_table_cell) @capture)
                        treesit-range-settings))))))

(ert-deftest thb-md-render-image-description-uses-real-parser-boundaries ()
  (skip-unless (treesit-language-available-p 'markdown-inline))
  (let ((source (generate-new-buffer " *thb-md-render-image-source*")))
    (unwind-protect
        (with-current-buffer source
          (insert "![foo](bar)")
          (let* ((parser (treesit-parser-create 'markdown-inline))
                 (root (treesit-parser-root-node parser))
                 (image (thb-md-render--first-child-of-type root "image"))
                 (description
                  (and image
                       (thb-md-render--first-child-of-type
                        image "image_description"))))
            (should image)
            (should description)
            ;; The grammar excludes ! from image_description and includes
            ;; both brackets: ![foo](bar) gives the half-open range 2..7.
            (should (= (treesit-node-start description) 2))
            (should (= (treesit-node-end description) 7))
            (should (equal (buffer-substring-no-properties
                            (treesit-node-start description)
                            (treesit-node-end description))
                           "[foo]"))
            (with-temp-buffer
              (let ((thb-md-render--src-buffer source))
                (thb-md-render--emit-inline-node image))
              (should (equal (buffer-string) "🖼 foo bar")))))
      (kill-buffer source))))

(ert-deftest thb-md-render-first-child-stops-at-first-match ()
  (let* ((first '(:type "other"))
         (match '(:type "wanted"))
         (unvisited '(:type "wanted"))
         (parent (list :children (list first match unvisited)))
         (visits 0))
    (cl-letf (((symbol-function 'treesit-node-child-count)
               (lambda (_node &optional _named) 3))
              ((symbol-function 'treesit-node-child)
               (lambda (node index &optional _named)
                 (cl-incf visits)
                 (nth index (plist-get node :children))))
              ((symbol-function 'treesit-node-type)
               (lambda (node) (plist-get node :type))))
      (should (eq (thb-md-render--first-child-of-type parent "wanted") match))
      (should (= visits 2)))))

(ert-deftest thb-md-render-plain-ranges-insert-without-source-properties ()
  (let ((source (generate-new-buffer " *thb-md-render-source-test*")))
    (unwind-protect
        (progn
          (with-current-buffer source
            (insert (propertize "plain" 'source-only t)))
          (with-temp-buffer
            (let ((thb-md-render--src-buffer source)
                  (thb-md-render--inline-face-stack nil))
              (thb-md-render--emit-plain 1 6))
            (should (equal (buffer-string) "plain"))
            (should-not (get-text-property 1 'source-only))
            (should (eq (get-text-property 1 'face) 'thb-md-render-body))))
      (kill-buffer source))))

(ert-deftest thb-md-render-heading-scales-drive-rendered-face ()
  (let ((thb-md-render-heading-scales '(3.0 2.0)))
    (should (equal (thb-md-render--heading-face 1)
                   '(:inherit thb-md-render-h1 :height 3.0)))
    (should (equal (thb-md-render--heading-face 2)
                   '(:inherit thb-md-render-h2 :height 2.0)))
    (should (equal (thb-md-render--heading-face 3)
                   '(:inherit thb-md-render-h3 :height 1.0)))))

(ert-deftest thb-md-render-list-state-preserves-start-and-marker-style ()
  (thb-md-render-test--with-fake-nodes
    (let* ((dot '(:type "list_marker_dot" :text "5."))
           (paren '(:type "list_marker_parenthesis" :text "12)"))
           (dot-list (list :children
                           (list (list :type "list_item"
                                       :children (list dot)))))
           (paren-list (list :children
                             (list (list :type "list_item"
                                         :children (list paren))))))
      (should (equal (thb-md-render--list-entry dot-list)
                     '(ordered 5 ".")))
      (should (equal (thb-md-render--list-entry paren-list)
                     '(ordered 12 ")"))))))

(ert-deftest thb-md-render-task-prefix-has-safe-per-state-fallbacks ()
  (thb-md-render-test--with-fake-nodes
    (let ((thb-md-render-task-glyphs '(("checked" . "YES "))))
      (should (equal (thb-md-render--task-prefix
                      '(:type "task_list_marker_checked"))
                     "YES "))
      (should (equal (thb-md-render--task-prefix
                      '(:type "task_list_marker_unchecked"))
                     "[ ] ")))))

(ert-deftest thb-md-render-lists-keep-markers-and-nesting-tight ()
  (thb-md-render-test--with-fake-nodes
    (let* ((ordered-marker '(:type "list_marker_parenthesis" :text "5)"))
           (unordered-marker '(:type "list_marker_minus" :text "-"))
           (paragraph-1 '(:type "paragraph" :children
                          ((:type "inline" :start 1 :end 2))))
           (paragraph-2 '(:type "paragraph" :children
                          ((:type "inline" :start 2 :end 3))))
           (paragraph-3 '(:type "paragraph" :children
                          ((:type "inline" :start 3 :end 4))))
           (nested (list :type "list" :children
                         (list (list :type "list_item" :children
                                     (list unordered-marker paragraph-2)))))
           (ordered (list :type "list" :children
                          (list (list :type "list_item" :children
                                      (list ordered-marker paragraph-1))
                                (list :type "list_item" :children
                                      (list '(:type "list_marker_parenthesis"
                                              :text "6)")
                                            paragraph-2)))))
           (outer (list :type "list" :children
                        (list (list :type "list_item" :children
                                    (list unordered-marker paragraph-1 nested))
                              (list :type "list_item" :children
                                    (list unordered-marker paragraph-3))))))
      (cl-letf (((symbol-function 'thb-md-render--inline-walk)
                 (lambda (start _end)
                   (insert (alist-get start '((1 . "outer")
                                              (2 . "nested")
                                              (3 . "next")))))))
        (with-temp-buffer
          (let ((thb-md-render--list-state nil))
            (thb-md-render--walk-list ordered))
          (should (equal (buffer-string) "5) outer\n6) nested\n\n")))
        (with-temp-buffer
          (let ((thb-md-render--list-state nil))
            (thb-md-render--walk-list outer))
          (should (equal (buffer-string)
                         "• outer\n  • nested\n• next\n\n")))))))

(ert-deftest thb-md-render-marker-only-parent-separates-nested-marker ()
  (thb-md-render-test--with-fake-nodes
    (let* ((marker '(:type "list_marker_minus" :text "-"))
           (paragraph '(:type "paragraph" :children
                        ((:type "inline" :start 1 :end 2))))
           (nested (list :type "list" :children
                         (list (list :type "list_item" :children
                                     (list marker paragraph)))))
           (outer (list :type "list" :children
                        (list (list :type "list_item" :children
                                    (list marker nested))))))
      (cl-letf (((symbol-function 'thb-md-render--inline-walk)
                 (lambda (_start _end) (insert "nested"))))
        (with-temp-buffer
          (let ((thb-md-render--list-state nil))
            (thb-md-render--walk-list outer))
          (should (equal (buffer-string) "• \n  • nested\n\n"))
          (should-not (string-match-p "• +•" (buffer-string))))))))

(defun thb-md-render-test--face-has-p (position face)
  "Return non-nil when POSITION's face property contains FACE."
  (memq face (flatten-tree (get-text-property position 'face))))

(ert-deftest thb-md-render-blockquote-prefix-is-rendered-and-faced ()
  (with-temp-buffer
    (let ((thb-md-render-blockquote-prefix ">> "))
      (cl-letf (((symbol-function 'treesit-node-children)
                 (lambda (_node &optional _named) '(child)))
                ((symbol-function 'treesit-node-type)
                 (lambda (_node) "paragraph"))
                ((symbol-function 'thb-md-render--walk)
                 (lambda (_node) (insert "quoted\n\n"))))
        (thb-md-render--walk-blockquote 'quote)))
    (should (equal (buffer-string) ">> quoted\n\n"))
    (should (thb-md-render-test--face-has-p
             1 'thb-md-render-blockquote-marker))
    (should (thb-md-render-test--face-has-p
             1 'thb-md-render-blockquote))
    (should (equal (get-text-property 1 'wrap-prefix) ">> "))))

(ert-deftest thb-md-render-thematic-break-uses-body-width ()
  (with-temp-buffer
    (let ((thb-md-render-body-width 17))
      (cl-letf (((symbol-function 'get-buffer-window)
                 (lambda (&rest _) nil)))
        (should (= (thb-md-render--thematic-break-width) 17))
        (thb-md-render--walk-thematic-break)))
    (should (equal (buffer-string)
                   (concat (make-string 17 ?─) "\n\n")))))

(defun thb-md-render-test-mode ()
  "Minimal mode used to exercise the fontification cache."
  (kill-all-local-variables)
  (setq major-mode 'thb-md-render-test-mode))

(defun thb-md-render-test-error-mode ()
  "Mode that always fails during initialization."
  (error "mode initialization failed"))

(ert-deftest thb-md-render-fontification-normalizes-language-and-refontifies ()
  (thb-md-render-fontify-cleanup)
  (let ((thb-md-render-language-mode-alist
         '(("python" . thb-md-render-test-mode)
           ("js" . thb-md-render-test-mode)))
        (fontify-count 0))
    (unwind-protect
        (cl-letf (((symbol-function 'font-lock-flush) #'ignore)
                  ((symbol-function 'font-lock-ensure)
                   (lambda (&rest _)
                     (cl-incf fontify-count)
                     (put-text-property (point-min) (point-max)
                                        'face 'font-lock-keyword-face)
                     (put-text-property (point-min) (point-max)
                                        'fontified t))))
          (dolist (language '("python" "Python" "PYTHON" "Js" "js"))
            (let ((result (thb-md-render--fontify-code "token" language)))
              (should (equal result "token"))
              (should (eq (get-text-property 0 'face result)
                          'font-lock-keyword-face))
              (should-not (get-text-property 0 'fontified result))))
          (should (= fontify-count 5))
          (should (equal (thb-md-render--fontify-code "plain" "unknown")
                         "plain")))
      (thb-md-render-fontify-cleanup))))

(ert-deftest thb-md-render-fontification-mode-error-leaks-no-buffer ()
  (thb-md-render-fontify-cleanup)
  (let ((thb-md-render-language-mode-alist
         '(("broken" . thb-md-render-test-error-mode)))
        (name " *thb-md-fontify-cache: thb-md-render-test-error-mode*"))
    (should (equal (thb-md-render--fontify-code "plain" "broken") "plain"))
    (should-not (gethash 'thb-md-render-test-error-mode
                         thb-md-render--fontify-buffers))
    (should-not (get-buffer name))))

(ert-deftest thb-md-render-fontified-strings-retain-only-face ()
  (with-temp-buffer
    (insert (propertize "x" 'face 'bold 'fontified t
                        'syntax-table '(1) 'composition '((0 . 1))))
    (let ((result (thb-md-render--fontify-face-only-string)))
      (should (eq (get-text-property 0 'face result) 'bold))
      (should-not (get-text-property 0 'fontified result))
      (should-not (get-text-property 0 'syntax-table result))
      (should-not (get-text-property 0 'composition result)))))

(ert-deftest thb-md-render-table-cells-reuse-clean-scratch-storage ()
  (let ((scratch (generate-new-buffer " *thb-md-table-cell-test*")))
    (unwind-protect
        (let ((thb-md-render--table-cell-buffer scratch))
          (cl-letf (((symbol-function 'treesit-node-start)
                     (lambda (node) (plist-get node :start)))
                    ((symbol-function 'treesit-node-end)
                     (lambda (node) (plist-get node :end)))
                    ((symbol-function 'thb-md-render--inline-walk)
                     (lambda (start _end)
                       (insert (propertize (format " cell-%d " start)
                                           'face 'italic)))))
            (let ((first (thb-md-render--table-render-cell
                          '(:start 1 :end 2)))
                  (second (thb-md-render--table-render-cell
                           '(:start 2 :end 3))))
              (should (equal first "cell-1"))
              (should (equal second "cell-2"))
              (should (eq (get-text-property 0 'face first) 'italic))
              (should (eq (get-text-property 0 'face second) 'italic))
              (with-current-buffer scratch
                (should (equal (buffer-string) " cell-2 "))))))
      (kill-buffer scratch))
    (should-not (buffer-live-p scratch))))

(ert-deftest thb-md-render-integration-ranges-lists-tables-and-quotes ()
  (skip-unless (and (treesit-language-available-p 'markdown)
                    (treesit-language-available-p 'markdown-inline)))
  (let ((file (make-temp-file "thb-md-render" nil ".md"))
        (thb-md-render-body-width 24)
        (thb-md-render-task-glyphs '(("checked" . "DONE ")))
        output)
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "```PYTHON\n*code must stay literal*\n```\n\n"
                    "A *prose token*.\n\n"
                    "| Head |\n| --- |\n| *cell token* |\n\n"
                    "5) five\n6) six\n\n"
                    "- [ ] pending\n- outer\n  - nested\n- next\n\n"
                    "> quoted\n\n---\n"))
          (setq output (thb-md-render-file file))
          (with-current-buffer output
            (goto-char (point-min))
            (search-forward "*code must stay literal*")
            (should-not (thb-md-render-test--face-has-p
                         (match-beginning 0) 'thb-md-render-emphasis))
            (should (thb-md-render-test--face-has-p
                     (match-beginning 0) 'thb-md-render-code-block))
            (should (= (cl-count 'thb-md-render-code-block
                                 (flatten-tree
                                  (get-text-property (match-beginning 0)
                                                     'face)))
                       1))
            (should-not (get-text-property (match-beginning 0) 'fontified))
            (should-not (get-text-property (match-beginning 0) 'syntax-table))
            (search-forward "prose token")
            (should (thb-md-render-test--face-has-p
                     (match-beginning 0) 'thb-md-render-emphasis))
            (search-forward "cell token")
            (should (thb-md-render-test--face-has-p
                     (match-beginning 0) 'thb-md-render-emphasis))
            (should (thb-md-render-test--face-has-p
                     (match-beginning 0) 'thb-md-render-table))
            (goto-char (point-min))
            (should (search-forward "5) five\n6) six" nil t))
            (should (search-forward "[ ] pending" nil t))
            (should (search-forward "• outer\n  • nested\n• next" nil t))
            (should (search-forward
                     (concat thb-md-render-blockquote-prefix "quoted") nil t))
            (goto-char (point-min))
            (re-search-forward (format "^%s$" (make-string 24 ?─)))
            (should (= (length (match-string 0)) 24))))
      (when (buffer-live-p output)
        (kill-buffer output))
      (delete-file file))))

(provide 'thb-markdown-render-test)
;;; thb-markdown-render-test.el ends here
