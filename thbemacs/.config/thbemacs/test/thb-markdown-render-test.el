;;; thb-markdown-render-test.el --- Tests for thb-markdown-render  -*- lexical-binding: t; -*-

(require 'ert)
(require 'benchmark)
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
            ;; The grammar excludes ! and both brackets from
            ;; image_description: ![foo](bar) gives the half-open range 3..6.
            (should (= (treesit-node-start description) 3))
            (should (= (treesit-node-end description) 6))
            (should (equal (buffer-substring-no-properties
                            (treesit-node-start description)
                            (treesit-node-end description))
                           "foo"))
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

(defmacro thb-md-render-test--with-render-stubs (&rest body)
  "Run BODY with parsing and layout reduced to deterministic unit seams."
  (declare (indent 0) (debug t))
  `(cl-letf (((symbol-function 'treesit-parser-create) #'ignore)
             ((symbol-function 'treesit-range-rules) (lambda (&rest _args) nil))
             ((symbol-function 'treesit-parser-list)
              (lambda (&rest _args) '(parser)))
             ((symbol-function 'treesit-parser-root-node) #'identity)
             ((symbol-function 'treesit-node-children) (lambda (_node) nil))
             ((symbol-function 'thb-md-render-mode)
              (lambda ()
                (setq major-mode 'thb-md-render-mode)
                (setq buffer-read-only t)))
             ((symbol-function 'thb-md-render--walk)
              (lambda (_root) (insert "rendered\n")))
             ((symbol-function 'thb-md-render--apply-olivetti-width) #'ignore)
             ((symbol-function 'thb-md-render--rewrap-prose) #'ignore)
             ((symbol-function 'get-buffer-window) (lambda (&rest _args) nil)))
     ,@body))

(ert-deftest thb-md-render-file-keeps-same-basename-sources-distinct ()
  (let* ((root (make-temp-file "thb-md-render-test-" t))
         (dir-a (expand-file-name "a" root))
         (dir-b (expand-file-name "b" root))
         (file-a (expand-file-name "README.md" dir-a))
         (file-b (expand-file-name "README.md" dir-b))
         buffers
         displayed)
    (unwind-protect
        (progn
          (make-directory dir-a)
          (make-directory dir-b)
          (with-temp-file file-a (insert "alpha"))
          (with-temp-file file-b (insert "beta"))
          (thb-md-render-test--with-render-stubs
            (cl-letf (((symbol-function 'pop-to-buffer)
                       (lambda (buffer &rest _args)
                         (push buffer displayed)
                         buffer)))
              (let ((preview-a (thb-md-render-file file-a))
                    (preview-b (thb-md-render-file file-b)))
                (setq buffers (list preview-a preview-b))
                (should-not (eq preview-a preview-b))
                (should-not (equal (buffer-name preview-a)
                                   (buffer-name preview-b)))
                (should (equal (buffer-local-value
                                'thb-md-render--source-file preview-a)
                               (file-truename file-a)))
                (should (equal (buffer-local-value
                                'thb-md-render--source-file preview-b)
                               (file-truename file-b)))
                ;; Ordinary Lisp calls return previews without displaying them.
                (should-not displayed)
                ;; Batch Emacs reports `called-interactively-p' as nil even
                ;; under `call-interactively', so control that production seam
                ;; explicitly while still exercising the interactive file read.
                (cl-letf (((symbol-function 'read-file-name)
                           (lambda (&rest _args) file-a))
                          ((symbol-function 'called-interactively-p)
                           (lambda (&optional _kind) t)))
                  (should (eq (call-interactively #'thb-md-render-file)
                              preview-a))
                  (should (equal displayed (list preview-a))))))))
      (mapc (lambda (buffer)
              (when (buffer-live-p buffer)
                (kill-buffer buffer)))
            buffers)
      (delete-directory root t))))

(ert-deftest thb-md-render-revert-uses-rendered-buffer-directly ()
  (let ((preview (generate-new-buffer " *thb-md-revert-target*"))
        (basename-buffer (generate-new-buffer "*md render: README.md*")))
    (unwind-protect
        (with-current-buffer preview
          (setq-local thb-md-render--source-file "/tmp/example/README.md")
          (insert "abcdefghij")
          (goto-char 6)
          (cl-letf (((symbol-function 'thb-md-render-file)
                     (lambda (_source)
                       (with-current-buffer preview
                         (erase-buffer)
                         (insert "aa\nbb\ncc\n"))
                       preview)))
            (thb-md-render-revert)
            (should (= (point) 4))
            (should (= (with-current-buffer basename-buffer (point)) 1))))
      (kill-buffer preview)
      (kill-buffer basename-buffer))))

(ert-deftest thb-md-render-watch-debounces-reattaches-and-cleans-up ()
  (let ((preview (generate-new-buffer " *thb-md-watch-test*"))
        callbacks scheduled cancelled removed
        (watch-count 0)
        (timer-count 0)
        (render-count 0))
    (unwind-protect
        (cl-letf (((symbol-function 'file-notify-add-watch)
                   (lambda (_source _flags callback)
                     (let ((descriptor
                            (intern (format "watch-%d" (cl-incf watch-count)))))
                       (push (cons descriptor callback) callbacks)
                       descriptor)))
                  ((symbol-function 'file-notify-rm-watch)
                   (lambda (descriptor) (push descriptor removed)))
                  ((symbol-function 'run-at-time)
                   (lambda (_delay _repeat function &rest args)
                     (let ((timer
                            (intern (format "timer-%d" (cl-incf timer-count)))))
                       (push (list timer function args) scheduled)
                       timer)))
                  ((symbol-function 'timerp) (lambda (value) (and value t)))
                  ((symbol-function 'cancel-timer)
                   (lambda (timer) (push timer cancelled)))
                  ((symbol-function 'file-readable-p) (lambda (_source) t))
                  ((symbol-function 'thb-md-render-revert)
                   (lambda () (cl-incf render-count))))
          (with-current-buffer preview
            (setq-local thb-md-render--source-file "/tmp/source.md")
            (thb-md-render--ensure-watch preview)
            (thb-md-render--ensure-watch preview)
            (should (= watch-count 1))
            (should (= (cl-count #'thb-md-render--cleanup kill-buffer-hook) 1)))

          (let ((callback (cdr (assq 'watch-1 callbacks))))
            (funcall callback '(watch-1 changed))
            (let ((first-timer (caar scheduled)))
              (funcall callback '(watch-1 attribute-changed))
              (should (memq first-timer cancelled)))
            (should (= render-count 0))
            (pcase-let ((`(,_timer ,function ,args) (car scheduled)))
              (apply function args))
            (should (= render-count 1))
            (should (eq (buffer-local-value 'thb-md-render--watch preview)
                        'watch-1))

            (funcall callback '(watch-1 renamed))
            (should (memq 'watch-1 removed))
            (should-not (buffer-local-value 'thb-md-render--watch preview))
            (pcase-let ((`(,_timer ,function ,args) (car scheduled)))
              (apply function args))
            (should (= watch-count 2))
            (should (= render-count 2))
            (should (eq (buffer-local-value 'thb-md-render--watch preview)
                        'watch-2))

            ;; A queued event from the disposed descriptor must not schedule
            ;; another render after the replacement takes ownership.
            (let ((scheduled-count (length scheduled)))
              (funcall callback '(watch-1 changed))
              (should (= (length scheduled) scheduled-count))))

          (with-current-buffer preview
            (setq thb-md-render--watch-timer 'pending-watch-timer)
            (setq thb-md-render--reflow-timer 'pending-reflow-timer))
          (kill-buffer preview)
          (should (memq 'watch-2 removed))
          (should (memq 'pending-watch-timer cancelled))
          (should (memq 'pending-reflow-timer cancelled)))
      (when (buffer-live-p preview)
        (kill-buffer preview)))))

(ert-deftest thb-md-render-theme-hook-is-idempotent-and-refreshes-faces ()
  (add-hook 'enable-theme-functions #'thb-md-render-apply-theme)
  (should (= (cl-count #'thb-md-render-apply-theme enable-theme-functions) 1))
  (let (updates)
    (cl-letf (((symbol-function 'featurep)
               (lambda (feature) (eq feature 'modus-themes)))
              ((symbol-function 'modus-themes-get-color-value)
               (lambda (color)
                 (pcase color
                   ('bg-dim "code-bg")
                   ('bg-blue-nuanced "quote-bg"))))
              ((symbol-function 'set-face-attribute)
               (lambda (face _frame &rest attributes)
                 (push (cons face attributes) updates))))
      (thb-md-render-apply-theme 'modus-vivendi)
      (should (equal (cdr (assq 'thb-md-render-code-block updates))
                     '(:background "code-bg")))
      (should (equal (cdr (assq 'thb-md-render-blockquote updates))
                     '(:background "quote-bg"))))))

(defun thb-md-render-test--character-width (string &optional _buffer)
  "Deterministic pixel-width stand-in that gives each character width one."
  (length string))

(defun thb-md-render-test--legacy-pixel-wrap (s budget cont-prefix)
  "Pre-change quadratic wrapper retained for output and benchmark comparison."
  (let* ((lead-len (or (string-match "[^ ]" s) (length s)))
         (lead (substring s 0 lead-len))
         (words (thb-md-render--split-words (substring s lead-len))))
    (if (null words)
        s
      (let ((lines nil) (cur lead) (has nil))
        (dolist (word words)
          (let ((candidate (if has
                               (concat cur (thb-md-render--sep-space word) word)
                             (concat cur word))))
            (if (<= (string-pixel-width candidate (current-buffer))
                    budget)
                (setq cur candidate has t)
              (if has
                  (progn
                    (push cur lines)
                    (setq cur (concat cont-prefix word) has t))
                (setq cur candidate has t)))))
        (push cur lines)
        (let ((lines (nreverse lines)))
          (if (null (cdr lines))
              (car lines)
            (let ((result (car lines)) (previous (car lines)))
              (dolist (line (cdr lines))
                (setq result
                      (concat result
                              (thb-md-render--faced-newline previous)
                              line)
                      previous line))
              result)))))))

(ert-deftest thb-md-render-pixel-wrap-preserves-output-and-properties ()
  (let* ((source (propertize "  alpha  beta gamma" 'face 'body-face))
         (prefix (propertize "->" 'face 'prefix-face))
         expected actual)
    (put-text-property 9 13 'face 'strong-face source)
    (put-text-property 14 19 'help-echo "gamma property" source)
    (cl-letf (((symbol-function 'string-pixel-width)
               #'thb-md-render-test--character-width))
      (setq expected (thb-md-render-test--legacy-pixel-wrap source 12 prefix)
            actual (thb-md-render--pixel-wrap source 12 prefix)))
    (should (equal-including-properties actual expected))
    (should (equal (substring-no-properties actual)
                   "  alpha beta\n->gamma"))
    (should (eq (get-text-property 8 'face actual) 'strong-face))
    (should (eq (get-text-property 12 'face actual) 'strong-face))
    (should (equal (get-text-property 15 'help-echo actual)
                   "gamma property"))))

(ert-deftest thb-md-render-pixel-wrap-keeps-overlong-words ()
  (cl-letf (((symbol-function 'string-pixel-width)
             #'thb-md-render-test--character-width))
    (should (equal (thb-md-render--pixel-wrap "abcdefgh ij" 4 "  ")
                   "abcdefgh\n  ij"))
    (should (equal (thb-md-render--pixel-wrap "   " 1 "->") "   "))))

(ert-deftest thb-md-render-measures-with-render-buffer-remapping ()
  (with-temp-buffer
    (let ((render-buffer (current-buffer))
          observed-buffer)
      (cl-letf (((symbol-function 'string-pixel-width)
                 (lambda (string &optional buffer)
                   (setq observed-buffer buffer)
                   (* 2 (length string)))))
        (should (= (thb-md-render--string-pixel-width "scaled") 12))
        (should (eq observed-buffer render-buffer))
        (setq observed-buffer nil)
        (let ((thb-md-render-body-width 3)
              (thb-md-render--wrap-window nil))
          (cl-letf (((symbol-function 'get-buffer-window) (lambda (&rest _) nil)))
            (should (= (thb-md-render--prose-budget-px) 6))
            (should (eq observed-buffer render-buffer))))))))

(ert-deftest thb-md-render-wraps-to-scaled-render-buffer-width ()
  (dolist (case '((-1 2) (0 3) (1 3)))
    (pcase-let ((`(,scale ,expected-lines) case))
      (with-temp-buffer
        (setq-local text-scale-mode-amount scale)
        (cl-letf (((symbol-function 'string-pixel-width)
                   (lambda (string &optional buffer)
                     (with-current-buffer buffer
                       (* (+ 2 text-scale-mode-amount)
                          (length string))))))
          (let ((wrapped (thb-md-render--pixel-wrap "aaa aaa aaa" 10 "")))
            (should (= (length (string-lines wrapped)) expected-lines))
            (dolist (line (string-lines wrapped))
              (should (<= (thb-md-render--string-pixel-width line) 10)))))))))

(ert-deftest thb-md-render-pixel-wrap-measurement-work-is-linear ()
  (let* ((word-count 1000)
         (source (mapconcat #'identity (make-list word-count "word") " "))
         (budget (1+ (length source)))
         legacy-work new-work)
    (cl-labels ((count-width
                 (string &optional _buffer)
                 (cl-incf legacy-work (length string))
                 (length string)))
      (setq legacy-work 0)
      (cl-letf (((symbol-function 'string-pixel-width) #'count-width))
        (thb-md-render-test--legacy-pixel-wrap source budget "")))
    (cl-labels ((count-width
                 (string &optional _buffer)
                 (cl-incf new-work (length string))
                 (length string)))
      (setq new-work 0)
      (cl-letf (((symbol-function 'string-pixel-width) #'count-width))
        (thb-md-render--pixel-wrap source budget "")))
    ;; The legacy pass measures every growing prefix (~N^2 characters), while
    ;; the new pass measures each word and separator once (~N characters).
    (should (> legacy-work (* 100 new-work)))
    ;; A tight budget exercises hundreds of forced line breaks.  Measurement
    ;; remains proportional to input length, and the constant continuation
    ;; prefix is measured once for the whole wrapping pass.
    (let ((cont-prefix ">>")
          (narrow-work 0)
          (prefix-measurements 0)
          narrow-result)
      (cl-labels ((count-width
                   (string &optional _buffer)
                   (cl-incf narrow-work (length string))
                   (when (equal string cont-prefix)
                     (cl-incf prefix-measurements))
                   (length string)))
        (cl-letf (((symbol-function 'string-pixel-width) #'count-width))
          (setq narrow-result
                (thb-md-render--pixel-wrap source 30 cont-prefix))))
      (should (> (length (string-lines narrow-result)) 150))
      (should (= prefix-measurements 1))
      (should (< narrow-work (* 2 word-count 5))))))

(ert-deftest thb-md-render-keeps-one-wrapping-window-owner ()
  (with-temp-buffer
    (let ((owner 'wide-window)
          (selected 'narrow-window)
          (buffer (current-buffer)))
      (setq thb-md-render--wrap-window owner)
      (cl-letf (((symbol-function 'window-live-p)
                 (lambda (window) (memq window '(wide-window narrow-window))))
                ((symbol-function 'window-buffer) (lambda (_window) buffer))
                ((symbol-function 'selected-window) (lambda () selected))
                ((symbol-function 'get-buffer-window)
                 (lambda (&rest _) selected)))
        (should (eq (thb-md-render--wrapping-window) owner))
        (setq thb-md-render--wrap-window 'dead-window)
        (should (eq (thb-md-render--wrapping-window) selected))))))

(ert-deftest thb-md-render-width-reflow-reuses-unwrapped-content ()
  (with-temp-buffer
    (let* ((source (propertize "one two three four five six seven eight"
                              'face 'body-face))
           (wrap-calls 0)
           (original-rewrap (symbol-function
                             'thb-md-render--rewrap-cached-content)))
      (insert source)
      (goto-char 20)
      (setq thb-md-render--unwrapped-content
            (buffer-substring (point-min) (point-max))
            thb-md-render--wrap-width 99)
      (cl-letf (((symbol-function 'thb-md-render--window-width-px)
                 (lambda () 12))
                ((symbol-function 'thb-md-render--prose-budget-px)
                 (lambda () 12))
                ((symbol-function 'thb-md-render--apply-olivetti-width)
                 #'ignore)
                ((symbol-function 'string-pixel-width)
                 #'thb-md-render-test--character-width)
                ((symbol-function 'thb-md-render-file)
                 (lambda (&rest _) (ert-fail "width reflow reparsed source")))
                ((symbol-function 'thb-md-render-revert)
                 (lambda (&rest _) (ert-fail "width reflow used full revert")))
                ((symbol-function 'thb-md-render--rewrap-cached-content)
                 (lambda ()
                   (cl-incf wrap-calls)
                   (funcall original-rewrap))))
        (thb-md-render--reflow-now)
        (should (= wrap-calls 1))
        (should (= thb-md-render--wrap-width 12))
        (should (string-match-p "\n" (buffer-string)))
        (should (> (point) (point-min)))
        (should (equal-including-properties
                 thb-md-render--unwrapped-content source))
        ;; The recorded owner width makes duplicate configuration events a
        ;; no-op, bounding the reflow count.
        (thb-md-render--reflow-now)
        (should (= wrap-calls 1))))))

(ert-deftest thb-md-render-margin-reset-feedback-does-not-schedule-reflow ()
  (with-temp-buffer
    (let ((scheduled 0)
          (thb-md-render--source-file "/tmp/example.md")
          (thb-md-render--unwrapped-content "content")
          (thb-md-render--reflowing t))
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (&rest _)
                   (cl-incf scheduled))))
        (thb-md-render--maybe-reflow)
        (should (zerop scheduled))))))

(defun thb-md-render-test-benchmark-pixel-wrap (&optional word-count budget)
  "Benchmark legacy and linear wrappers on WORD-COUNT words.
Interactively default to 5000 words.  BUDGET defaults to effectively unlimited;
pass a narrow pixel budget to include forced line breaks.  Return an alist of
elapsed seconds.  This is intentionally not an ERT assertion because wall-clock
ratios vary.

Reproduce both paths in batch from the thbemacs config directory with:
  emacs -Q --batch -L lisp -L test -l test/thb-markdown-render-test.el \\
    --eval '(prin1 (thb-md-render-test-benchmark-pixel-wrap 5000))'
  emacs -Q --batch -L lisp -L test -l test/thb-markdown-render-test.el \\
    --eval '(prin1 (thb-md-render-test-benchmark-pixel-wrap 5000 80))'"
  (interactive)
  (let* ((word-count (or word-count 5000))
         (source (mapconcat #'identity (make-list word-count "propertized") " "))
         (budget (or budget most-positive-fixnum))
         legacy new)
    (put-text-property 0 (length source) 'face 'thb-md-render-body source)
    (garbage-collect)
    (setq legacy (car (benchmark-run 3
                        (thb-md-render-test--legacy-pixel-wrap
                         source budget ""))))
    (garbage-collect)
    (setq new (car (benchmark-run 3
                     (thb-md-render--pixel-wrap source budget ""))))
    (let ((result `((words . ,word-count)
                    (budget . ,budget)
                    (legacy-seconds . ,legacy)
                    (linear-seconds . ,new)
                    (speedup . ,(if (zerop new) nil (/ legacy new))))))
      (when (called-interactively-p 'interactive)
        (message "%S" result))
      result)))

(provide 'thb-markdown-render-test)
;;; thb-markdown-render-test.el ends here
