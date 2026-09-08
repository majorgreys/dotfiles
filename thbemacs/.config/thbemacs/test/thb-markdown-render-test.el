;;; thb-markdown-render-test.el --- Renderer wrapping tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'benchmark)
(require 'cl-lib)

(add-to-list 'load-path
             (expand-file-name "../lisp" (file-name-directory load-file-name)))
(require 'thb-markdown-render)

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
