;;; thb-snapshot-test.el --- Frame snapshot tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(add-to-list 'load-path
             (expand-file-name "../lisp"
                               (file-name-directory (or load-file-name buffer-file-name))))
(require 'thb-snapshot)

(defmacro thb-snapshot-test--with-frame (&rest body)
  "Run BODY with a temporary output directory and a fake window capture."
  (declare (indent 0))
  `(let* ((directory (make-temp-file "thb-snapshot-test-" t))
          (output (expand-file-name "frame.png" directory))
          (frame (selected-frame))
          (payload (concat (unibyte-string #x89 ?P ?N ?G 13 10 26 10) "fixture"))
          captured)
     (unwind-protect
         (cl-letf (((symbol-function 'thb-open--normal-frame-p)
                    (lambda (candidate) (eq candidate frame)))
                   ((symbol-function 'thb-open--window-id)
                    (lambda (candidate) (should (eq candidate frame)) "42"))
                   ((symbol-function 'thb-snapshot--capture)
                    (lambda (id path)
                      (setq captured (list id path))
                      (with-temp-file path
                        (set-buffer-multibyte nil)
                        (insert payload)))))
           ,@body)
       (delete-directory directory t))))

(ert-deftest thb-snapshot-saves-only-after-capture-and-png-header-check ()
  (thb-snapshot-test--with-frame
    (should (equal (thb-snapshot-frame output frame) output))
    (should (equal (car captured) "42"))
    (should-not (equal (cadr captured) output))
    (should (thb-snapshot--png-p output))
    (should (= (logand (file-modes output) #o777) #o600))
    (should-not (directory-files directory nil "^\\.thb-snapshot-"))))

(ert-deftest thb-snapshot-supports-spaces-quotes-and-unicode-in-output ()
  (thb-snapshot-test--with-frame
    (setq output (expand-file-name "a 'quoted' λ frame.png" directory))
    (should (equal (thb-snapshot-frame output frame) output))
    (should (file-exists-p output))))

(ert-deftest thb-snapshot-rejects-non-gui-or-auxiliary-frames ()
  (thb-snapshot-test--with-frame
    (cl-letf (((symbol-function 'thb-open--normal-frame-p) (lambda (_) nil)))
      (should-error (thb-snapshot-frame output frame) :type 'user-error))
    (should-not captured)))

(ert-deftest thb-snapshot-rejects-remote-output-before-file-access ()
  (thb-snapshot-test--with-frame
    (cl-letf (((symbol-function 'file-attributes)
               (lambda (&rest _) (ert-fail "must reject remote path before file access"))))
      (should-error (thb-snapshot-frame "/ssh:never-connect:/tmp/frame.png" frame)
                    :type 'user-error))
    (should-not captured)))

(ert-deftest thb-snapshot-rejects-wrong-extension-and-missing-parent ()
  (thb-snapshot-test--with-frame
    (should-error (thb-snapshot-frame (expand-file-name "frame.jpg" directory) frame)
                  :type 'user-error)
    (should-error (thb-snapshot-frame (expand-file-name "missing/frame.png" directory) frame)
                  :type 'user-error)
    (should-not captured)))

(ert-deftest thb-snapshot-programmatic-calls-do-not-overwrite ()
  (thb-snapshot-test--with-frame
    (with-temp-file output (insert "original"))
    (should-error (thb-snapshot-frame output frame) :type 'user-error)
    (should (= (file-attribute-size (file-attributes output)) 8))
    (should-not captured)))

(ert-deftest thb-snapshot-interactive-overwrite-requires-affirmative-answer ()
  (thb-snapshot-test--with-frame
    (with-temp-file output (insert "original"))
    (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) output))
              ((symbol-function 'called-interactively-p) (lambda (_) t))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) nil)))
      (should-error (call-interactively #'thb-snapshot-frame) :type 'user-error))
    (should-not captured)
    (should (= (file-attribute-size (file-attributes output)) 8))))

(ert-deftest thb-snapshot-interactive-overwrite-after-approval ()
  (thb-snapshot-test--with-frame
    (with-temp-file output (insert "original"))
    (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) output))
              ((symbol-function 'called-interactively-p) (lambda (_) t))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
      (should (equal (call-interactively #'thb-snapshot-frame) output)))
    (should (thb-snapshot--png-p output))))

(ert-deftest thb-snapshot-prompt-stays-local-from-a-tramp-buffer ()
  (thb-snapshot-test--with-frame
    (let ((default-directory "/ssh:never-connect:/tmp/"))
      (cl-letf (((symbol-function 'read-file-name)
                 (lambda (&rest _)
                   (should-not (file-remote-p default-directory))
                   output)))
        (should (equal (call-interactively #'thb-snapshot-frame) output))))))

(ert-deftest thb-snapshot-failure-preserves-original-and-removes-temporary ()
  (thb-snapshot-test--with-frame
    (with-temp-file output (insert "original"))
    (cl-letf (((symbol-function 'read-file-name) (lambda (&rest _) output))
              ((symbol-function 'called-interactively-p) (lambda (_) t))
              ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'thb-snapshot--capture)
               (lambda (_ path)
                 (setq captured t)
                 (with-temp-file path (insert "partial"))
                 (error "Permission denied"))))
      (should-error (call-interactively #'thb-snapshot-frame)))
    (should captured)
    (should (= (file-attribute-size (file-attributes output)) 8))
    (should-not (directory-files directory nil "^\\.thb-snapshot-"))))

(ert-deftest thb-snapshot-invalid-or-empty-output-is-not-success ()
  (thb-snapshot-test--with-frame
    (dolist (contents '("" "not a PNG"))
      (cl-letf (((symbol-function 'thb-snapshot--capture)
                 (lambda (_ path) (with-temp-file path (insert contents)))))
        (should-error (thb-snapshot-frame output frame)))
      (should-not (file-exists-p output))
      (should-not (directory-files directory nil "^\\.thb-snapshot-")))))

(ert-deftest thb-snapshot-refuses-output-created-during-capture ()
  (thb-snapshot-test--with-frame
    (cl-letf (((symbol-function 'thb-snapshot--capture)
               (lambda (_ path)
                 (with-temp-file path (set-buffer-multibyte nil) (insert payload))
                 (with-temp-file output (insert "another writer")))))
      (should-error (thb-snapshot-frame output frame)))
    (should (= (file-attribute-size (file-attributes output)) 14))
    (should-not (directory-files directory nil "^\\.thb-snapshot-"))))

(ert-deftest thb-snapshot-rejects-symlink-and-directory-destinations ()
  (thb-snapshot-test--with-frame
    (let ((original (expand-file-name "original.txt" directory)))
      (with-temp-file original (insert "original"))
      (make-symbolic-link original output)
      (should-error (thb-snapshot-frame output frame) :type 'user-error)
      (delete-file output)
      (make-directory output)
      (should-error (thb-snapshot-frame output frame) :type 'user-error)
      (should-not captured))))

(defmacro thb-snapshot-test--with-process (&rest body)
  "Run BODY with a fake capture process; no OS screenshot is taken."
  (declare (indent 0))
  `(let (launch launch-directory output-buffer deleted
                 (live t) (exit-code 0) (launches 0))
     (cl-letf (((symbol-function 'make-process)
                (lambda (&rest args)
                  (setq launch args launch-directory default-directory
                        output-buffer (plist-get args :buffer)
                        launches (1+ launches))
                  'capture-process))
               ((symbol-function 'process-live-p) (lambda (_) live))
               ((symbol-function 'process-status) (lambda (_) 'exit))
               ((symbol-function 'process-exit-status) (lambda (_) exit-code))
               ((symbol-function 'accept-process-output)
                (lambda (process interval)
                  (should (eq process 'capture-process))
                  (should (= interval 0.05))
                  (setq live nil)))
               ((symbol-function 'delete-process)
                (lambda (process) (setq deleted process live nil)))
               ((symbol-function 'call-process)
                (lambda (&rest _) (ert-fail "must service Emacs events"))))
       ,@body)))

(ert-deftest thb-snapshot-process-is-local-window-only-and-shell-free ()
  (thb-snapshot-test--with-process
    (let ((default-directory "/ssh:never-connect:/tmp/")
          (file "/tmp/a 'quoted' λ.png"))
      (thb-snapshot--capture "42" file)
      (should (equal launch-directory "/"))
      (should (equal (plist-get launch :command)
                     (list thb-snapshot-program "-x" "-o" "-t" "png" "-l" "42" file)))
      (should (plist-get launch :noquery))
      (should (eq (plist-get launch :connection-type) 'pipe))
      (should-not deleted))))

(ert-deftest thb-snapshot-process-failure-does-not-retry-or-fall-back ()
  (thb-snapshot-test--with-process
    (setq exit-code 1)
    (should-error (thb-snapshot--capture "42" "/tmp/frame.png"))
    (should (= launches 1))))

(ert-deftest thb-snapshot-process-interruption-kills-only-its-own-process ()
  (thb-snapshot-test--with-process
    (cl-letf (((symbol-function 'accept-process-output)
               (lambda (&rest _) (error "Wait interrupted"))))
      (should-error (thb-snapshot--capture "42" "/tmp/frame.png")))
    (should (eq deleted 'capture-process))
    (should (= launches 1))))

(ert-deftest thb-snapshot-uses-a-new-image-path-in-a-private-staging-directory ()
  (thb-snapshot-test--with-frame
    (let ((capture (symbol-function 'thb-snapshot--capture)))
      (cl-letf (((symbol-function 'thb-snapshot--capture)
                 (lambda (id path)
                   (should-not (file-exists-p path))
                   (should (= (logand (file-modes (file-name-directory path)) #o777) #o700))
                   (funcall capture id path))))
        (should (equal (thb-snapshot-frame output frame) output))))
    (should-not (directory-files directory nil "^\\.thb-snapshot-"))))

(ert-deftest thb-snapshot-missing-output-reports-diagnostics-and-cleans-up ()
  (thb-snapshot-test--with-frame
    (cl-letf (((symbol-function 'thb-snapshot--capture)
               (lambda (_ _path) "capture diagnostic")))
      (let ((failure (should-error (thb-snapshot-frame output frame))))
        (should (eq (car failure) 'error))
        (should (string-match-p "capture diagnostic" (error-message-string failure)))))
    (should-not (file-exists-p output))
    (should-not (directory-files directory nil "^\\.thb-snapshot-"))))

(ert-deftest thb-snapshot-png-check-rejects-missing-files-and-directories ()
  (thb-snapshot-test--with-frame
    (should-not (thb-snapshot--png-p output))
    (should-not (thb-snapshot--png-p directory))))

;;; thb-snapshot-test.el ends here
