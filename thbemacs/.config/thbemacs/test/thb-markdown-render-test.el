;;; thb-markdown-render-test.el --- Tests for markdown preview lifecycle -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)

(add-to-list 'load-path
             (expand-file-name "../lisp"
                               (file-name-directory
                                (or load-file-name buffer-file-name))))
(require 'thb-markdown-render)

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

(provide 'thb-markdown-render-test)
;;; thb-markdown-render-test.el ends here
