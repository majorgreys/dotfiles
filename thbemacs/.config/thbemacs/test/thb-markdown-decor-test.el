;;; thb-markdown-decor-test.el --- Tests for thb-markdown-decor  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;;
;; Run with:
;;
;;   emacs -Q --batch -l test/thb-markdown-decor-test.el
;;
;; Each `bN-' test pins a bug that was live in the pre-jit-lock version;
;; the remaining tests pin the reveal/rehide contract.  Tests drive
;; decoration through `flush' (what redisplay does) rather than
;; `font-lock-ensure', which no-ops once `font-lock-fontified' is set.
;;
;;; Code:

(setq treesit-extra-load-path (list (expand-file-name "~/.config/thbemacs/tree-sitter")))
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory (or load-file-name buffer-file-name))))
(require 'treesit) (require 'markdown-ts-mode) (require 'thb-markdown-decor)
(require 'ert) (require 'cl-lib)

(defun hid ()
  (let (out (pos (point-min)))
    (while (< pos (point-max))
      (let ((next (or (next-single-property-change pos 'invisible nil (point-max)) (point-max))))
        (when (eq (get-text-property pos 'invisible) 'thb-markdown-decor)
          (push (buffer-substring-no-properties pos next) out))
        (setq pos next)))
    (nreverse out)))

;; What redisplay does: run the jit-lock functions over anything marked dirty.
(defun flush () (jit-lock-fontify-now (point-min) (point-max)))

(defmacro with-md (text &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,text) (markdown-ts-mode) (font-lock-ensure)
     (thb-markdown-decor-mode 1) (flush)
     ,@body))

;;; B1 nested constructs.  Adjacent hidden ranges coalesce into one run, so
;;; e.g. the `*' of the emphasis and the `[' of the link inside it read as
;;; a single "*[".  Before the fix this was:
;;;   ("**" "**" "*" "*" "[" "](v)" "`" "`")   ; nested constructs missed
(ert-deftest b1-nested ()
  (with-md "**a `b` c** and *[l](u)* and [**q**](v) and `x`\n"
    (should (equal (hid)
                   '("**" "`" "`" "**" "*[" "](u)*" "[**" "**](v)" "`" "`")))))

(ert-deftest b1-still-flat ()
  (with-md "**foo**\n" (should (equal (hid) '("**" "**"))))
  (with-md "*a **b** c*\n" (should (equal (hid) '("*" "**" "**" "*"))))
  (with-md "~~x~~\n" (should (equal (hid) '("~~" "~~"))))
  (with-md "~single~ and ~~~triple~~~\n" (should (equal (hid) nil)))
  ;; markup inside a code span must stay literal
  (with-md "a *it `**x**` end* b\n" (should (equal (hid) '("*" "`" "`" "*")))))

;;; B2 + B3 edit inside a revealed node
(ert-deftest b2-reveal-survives-edit ()
  (with-md "hello **world** tail\n"
    (goto-char 12) (thb-markdown-decor--post-command)
    (should (equal (hid) nil))
    (insert "Q") (flush) (thb-markdown-decor--post-command)
    (should (equal (buffer-substring-no-properties (point-min) (point-max))
                   "hello **worQld** tail\n"))
    (should (equal (hid) nil))))                     ; still revealed

(ert-deftest b2-reveal-survives-full-refresh ()
  (with-md "x **bold** y\n"
    (goto-char 6) (thb-markdown-decor--post-command)
    (should (equal (hid) nil))
    (thb-markdown-decor--decorate-region (point-min) (point-max))
    (should (equal (hid) nil))))

(ert-deftest b3-edit-before-revealed-node ()
  (with-md "0123456789 **bold** tail\n"
    (goto-char 16) (thb-markdown-decor--post-command)
    (should (equal (hid) nil))
    (save-excursion (goto-char (point-min)) (delete-char 5))
    (flush) (thb-markdown-decor--post-command)
    (should (equal (hid) nil))                       ; markers tracked the shift
    (goto-char (point-max)) (thb-markdown-decor--post-command)
    (should (equal (hid) '("**" "**")))))            ; and re-hide on exit still works

(ert-deftest b2-typing-at-node-end ()
  (with-md "a **bo** z\n"
    (goto-char 7) (thb-markdown-decor--post-command)   ; inside "bo"
    (goto-char 8) (insert "x") (flush)                 ; type at end of "bo"
    (thb-markdown-decor--post-command)
    (should (equal (hid) nil))))

;;; B4 multi-line inline nodes
(ert-deftest b4-multiline-node ()
  (with-md "para with **bold\nacross lines** tail\n"
    (should (equal (hid) '("**" "**")))
    (goto-char (point-min)) (search-forward "lines**") (delete-char -2)
    (flush)
    (should (equal (hid) nil))))                     ; line 1's ** cleaned up too

;;; B5 no stray work after disable
(ert-deftest b5-no-work-after-disable ()
  (let ((buf (generate-new-buffer "b5")))
    (unwind-protect
        (with-current-buffer buf
          (insert "a **b** c\n") (markdown-ts-mode) (font-lock-ensure)
          (thb-markdown-decor-mode 1) (flush)
          (should (equal (hid) '("**" "**")))
          (thb-markdown-decor-mode -1)
          (should (equal (hid) nil))
          (should-not (memq #'thb-markdown-decor--fontify jit-lock-functions))
          ;; even a direct late call is inert
          (thb-markdown-decor--fontify (point-min) (point-max))
          (should (equal (hid) nil)))
      (kill-buffer buf))))

;;; B6 narrowing
(ert-deftest b6-narrowing ()
  ;; enable while narrowed, then widen: the rest still decorates
  (with-temp-buffer
    (insert "a **b** c\n\nd **e** f\n") (markdown-ts-mode) (font-lock-ensure)
    (narrow-to-region 1 11)
    (thb-markdown-decor-mode 1)
    (widen) (flush)
    (should (equal (hid) '("**" "**" "**" "**"))))
  ;; disable while narrowed: nothing survives outside the restriction
  (with-temp-buffer
    (insert "a **b** c\n\nd **e** f\n") (markdown-ts-mode) (font-lock-ensure)
    (thb-markdown-decor-mode 1) (flush)
    (should (equal (hid) '("**" "**" "**" "**")))
    (narrow-to-region 12 (point-max))
    (thb-markdown-decor-mode -1)
    (widen)
    (should (equal (hid) nil))))

;;; B7 clear removes the property instead of nil-ing it
(ert-deftest b7-clear-removes-property ()
  (with-md "a **b** c\n"
    (thb-markdown-decor-mode -1)
    (should-not (plist-member (text-properties-at 3) 'invisible))))

;;; B8 help-echo ownership
(ert-deftest b8-help-echo ()
  (with-md "see [txt](http://ex.com) end\n"
    (should (equal (get-text-property 6 'help-echo) "http://ex.com"))
    (thb-markdown-decor-mode -1)
    (should-not (get-text-property 6 'thb-markdown-decor-url)))
  ;; a pre-existing help-echo is neither clobbered nor deleted
  (with-temp-buffer
    (insert "see [txt](http://ex.com) end\n") (markdown-ts-mode) (font-lock-ensure)
    (put-text-property 6 8 'help-echo "someone else")
    (thb-markdown-decor-mode 1) (flush)
    (should (equal (get-text-property 6 'help-echo) "someone else"))
    (thb-markdown-decor-mode -1)
    (should (equal (get-text-property 6 'help-echo) "someone else"))))

;;; B9 enabling never errors
(ert-deftest b9-enable-never-errors ()
  (dolist (txt '("" "```\nx **y** z\n```\n" "| a | **b** |\n|---|---|\n| 1 | 2 |\n"
                 "    indented **code**\n" "hello **world**\n"))
    (with-temp-buffer
      (insert txt)
      (should (eq :ok (progn (markdown-ts-mode) (thb-markdown-decor-mode 1) :ok)))))
  ;; and it must not abort the rest of the mode hook
  (let ((ran nil))
    (add-hook 'markdown-ts-mode-hook #'thb-markdown-decor-mode)
    (add-hook 'markdown-ts-mode-hook (lambda () (setq ran t)) t)
    (unwind-protect
        (with-temp-buffer (insert "hi **there**\n") (markdown-ts-mode) (should ran))
      (setq markdown-ts-mode-hook nil))))

;;; B10 boundary between two adjacent nodes
(ert-deftest b10-adjacent-nodes ()
  (with-md "**a****b**\n"
    (goto-char 4) (thb-markdown-decor--post-command)   ; inside first "a"
    (let ((b (thb-markdown-decor--revealed-bounds)))
      (should (equal b '(1 . 6))))
    (goto-char 9) (thb-markdown-decor--post-command)   ; inside second "b"
    (should (equal (thb-markdown-decor--revealed-bounds) '(6 . 11)))))

;;; B11 post-command survives errors
(ert-deftest b11-post-command-demotes-errors ()
  (with-md "a **b** c\n"
    (cl-letf (((symbol-function 'thb-markdown-decor--decorated-parent-at)
               (lambda (_pos) (error "boom"))))
      (should (eq :ok (progn (thb-markdown-decor--post-command) :ok))))))

;;; B12 echo area not clobbered.  Batch Emacs does not maintain
;;; `current-message', so stub it and record what we would have echoed.
(ert-deftest b12-echo-respects-existing-message ()
  (with-md "see [t](http://ex.com) end\n"
    (let (echoed)
      (cl-letf (((symbol-function 'current-message) (lambda () "important"))
                ((symbol-function 'message)
                 (lambda (&rest args) (push args echoed))))
        (goto-char 6) (thb-markdown-decor--post-command))
      (should (equal echoed nil)))
    (let (echoed)
      (goto-char (point-max)) (thb-markdown-decor--post-command)
      (cl-letf (((symbol-function 'current-message) (lambda () nil))
                ((symbol-function 'message)
                 (lambda (&rest args) (push args echoed))))
        (goto-char 6) (thb-markdown-decor--post-command))
      (should (equal echoed '(("%s" "http://ex.com")))))))

;;; regressions on the existing reveal contract
(ert-deftest reveal-and-rehide ()
  (with-md "x **bold** y\n"
    (should (equal (hid) '("**" "**")))
    (goto-char 6) (thb-markdown-decor--post-command)
    (should (equal (hid) nil))
    (goto-char (point-max)) (thb-markdown-decor--post-command)
    (should (equal (hid) '("**" "**")))))

(ert-deftest reveal-not-in-plain-text ()
  (with-md "plain **b** plain\n"
    (goto-char 2) (thb-markdown-decor--post-command)
    (should (equal (hid) '("**" "**")))))

(ert-deftest node-deleted-while-revealed ()
  (with-md "x **bold** y\n"
    (goto-char 6) (thb-markdown-decor--post-command)
    (goto-char (point-min)) (search-forward "**") (delete-char -2)
    (flush) (goto-char (point-max)) (thb-markdown-decor--post-command)
    (should (equal (hid) nil))
    (should-not (thb-markdown-decor--revealed-bounds))))

(ert-deftest custom-type-list ()
  (with-temp-buffer
    (insert "**b** and `c`\n") (markdown-ts-mode) (font-lock-ensure)
    (setq-local thb-markdown-decor-decorated-types '("code_span"))
    (thb-markdown-decor-mode 1) (flush)
    (should (equal (hid) '("`" "`")))))

(ert-deftest toggle-is-idempotent ()
  (with-md "a **b** `c` [d](e)\n"
    (let ((before (buffer-string)))
      (dotimes (_ 3)
        (thb-markdown-decor-mode -1) (thb-markdown-decor-mode 1) (flush))
      (should (equal (hid) '("**" "**" "`" "`" "[" "](e)")))
      (thb-markdown-decor-mode -1)
      (should (equal (buffer-string) before)))))

;;; Review regressions

(ert-deftest rehide-under-narrowing ()
  ;; Moving off a revealed multi-line node while narrowed must re-hide it
  ;; once widened, not leave its markup visible and untracked.
  (with-md "aaa **bold\nmore** tail\n\nnext\n"
    (goto-char 7) (thb-markdown-decor--post-command)
    (should (equal (hid) nil))
    (narrow-to-region 1 12)
    (goto-char 2) (thb-markdown-decor--post-command)
    (widen)
    (should (equal (hid) '("**" "**")))
    (should-not thb-markdown-decor--revealed)))

(ert-deftest reveal-next-node-under-narrowing ()
  ;; Re-hiding under narrowing refreshes parser ranges, which outdates the
  ;; node found before it; the next node must still be revealed.
  (with-md "**a** **b** tail\n"
    (goto-char 3) (thb-markdown-decor--post-command)
    ;; Narrowed, but both nodes stay whole inside it.
    (narrow-to-region 1 12)
    (goto-char 9) (thb-markdown-decor--post-command)
    (widen)
    (should (equal (thb-markdown-decor--revealed-bounds) '(7 . 12)))
    (should (equal (hid) '("**" "**")))))

(ert-deftest nested-strikethrough-follows-policy ()
  ;; Only exact ~~double~~ strikethrough hides, at any nesting depth.
  (with-md "*a ~b~ c* and **x ~~~y~~~ z**\n"
    (should (equal (hid) '("*" "*" "**" "**"))))
  (with-md "*a ~~b~~ c*\n"
    (should (equal (hid) '("*" "~~" "~~" "*")))))

(ert-deftest custom-type-list-applies-to-nested-nodes ()
  (with-temp-buffer
    (insert "*a **b** c*\n") (markdown-ts-mode) (font-lock-ensure)
    (setq-local thb-markdown-decor-decorated-types '("emphasis"))
    (thb-markdown-decor-mode 1) (flush)
    (should (equal (hid) '("*" "*")))))

(ert-deftest revealed-end-follows-typing ()
  ;; The end marker must advance with text typed at the node's end, before
  ;; post-command re-reads the live node.
  (with-md "hello **world** tail\n"
    (goto-char 12) (thb-markdown-decor--post-command)
    (let ((end (cdr (thb-markdown-decor--revealed-bounds))))
      (goto-char end) (insert "!")
      (should (= (cdr (thb-markdown-decor--revealed-bounds)) (1+ end))))))

(ert-deftest delimiters-inside-literal-nodes-stay ()
  ;; The real grammar never nests delimiters in a code span, so pin the
  ;; guard with fake nodes: (strong (delim) (code_span (delim)) (delim)).
  (let* ((leaf (lambda (type start end) (list :type type :start start :end end)))
         (tree (list :type "strong_emphasis" :start 1 :end 14
                     :children (list (funcall leaf "emphasis_delimiter" 1 3)
                                     (list :type "code_span" :start 4 :end 11
                                           :children (list (funcall leaf "emphasis_delimiter" 6 7)))
                                     (funcall leaf "emphasis_delimiter" 12 14)))))
    (with-temp-buffer
      (insert "**a `b*c` d**\n")
      (cl-letf (((symbol-function 'treesit-node-type) (lambda (n) (plist-get n :type)))
                ((symbol-function 'treesit-node-start) (lambda (n) (plist-get n :start)))
                ((symbol-function 'treesit-node-end) (lambda (n) (plist-get n :end)))
                ((symbol-function 'treesit-node-child-count)
                 (lambda (n &optional _) (length (plist-get n :children))))
                ((symbol-function 'treesit-node-child)
                 (lambda (n i &optional _) (nth i (plist-get n :children)))))
        (thb-markdown-decor--hide-delimiters tree "emphasis_delimiter"))
      (should (equal (hid) '("**" "**"))))))

(when noninteractive (ert-run-tests-batch-and-exit))

(provide 'thb-markdown-decor-tests)
;;; thb-markdown-decor-test.el ends here
