;;; thb-markdown-render-terminal-test.el --- Preview regressions -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Run from this repository's thbemacs/.config/thbemacs directory, with the
;; installed grammars on the path:
;;   emacs -Q --batch --eval '(setq treesit-extra-load-path (list (expand-file-name "~/.config/thbemacs/tree-sitter")))' \
;;     -l ert -l test/thb-markdown-render-terminal-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(load (expand-file-name "../lisp/thb-markdown-render.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(defmacro thb-md-test--with-render (markdown &rest body)
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

(ert-deftest thb-md-render-decodes-prose-and-table-escapes ()
  (thb-md-test--with-render
      "Prose retention\\_filter, \\*literal\\*, \\\\ and \\q.\n\n| retained\\_by | code |\n| --- | --- |\n| trace\\_filter | `literal\\_code` |\n\n```text\nblock\\_code\n```\n"
    (let ((text (buffer-string)))
      (should (string-match-p (regexp-quote "retention_filter, *literal*, \\ and \\q.") text))
      (should (string-match-p (regexp-quote "retained_by") text))
      (should (string-match-p (regexp-quote "trace_filter") text))
      (should (string-match-p (regexp-quote "literal\\_code") text))
      (should (string-match-p (regexp-quote "block\\_code") text)))))

(ert-deftest thb-md-render-mode-installs-automatic-reflow-hook ()
  (with-temp-buffer
    (thb-md-render-mode)
    (should (memq #'thb-md-render--maybe-reflow
                  window-configuration-change-hook))))

(ert-deftest thb-md-render-terminal-budget-uses-columns ()
  (cl-letf (((symbol-function 'thb-md-render--window-width-px) (lambda () 72))
            ((symbol-function 'thb-md-render--wrapping-window) #'selected-window)
            ((symbol-function 'display-graphic-p) (lambda (&optional _) nil))
            ((symbol-function 'thb-md-render--string-pixel-width) #'string-width))
    (should (= (thb-md-render--prose-budget-px) 71))))

(ert-deftest thb-md-render-terminal-measures-display-columns ()
  (cl-letf (((symbol-function 'thb-md-render--wrapping-window) #'selected-window)
            ((symbol-function 'display-graphic-p) (lambda (&optional _) nil))
            ((symbol-function 'string-pixel-width) (lambda (&rest _) 999)))
    (should (= (thb-md-render--string-pixel-width "A界") 3))))

(ert-deftest thb-md-render-graphical-measurement-keeps-pixels ()
  (cl-letf (((symbol-function 'thb-md-render--wrapping-window) #'selected-window)
            ((symbol-function 'display-graphic-p) (lambda (&optional _) t))
            ((symbol-function 'string-pixel-width) (lambda (&rest _) 123)))
    (should (= (thb-md-render--string-pixel-width "example") 123))))

(ert-deftest thb-md-render-terminal-reflows-prose-but-not-tables-or-code ()
  (thb-md-test--with-render
      (concat (mapconcat (lambda (_) "readable prose") (number-sequence 1 25) " ")
              "\n\n- " (mapconcat (lambda (_) "list item") (number-sequence 1 25) " ")
              "\n\n| column | other |\n| --- | --- |\n| " (make-string 100 ?x) " | value |\n"
              "\n```text\n" (make-string 100 ?c) "\n```\n")
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (dolist (columns '(72 40 100))
        (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) columns))
                  ((symbol-function 'display-graphic-p) (lambda (&optional _) nil)))
          (thb-md-render--rewrap-cached-content)
          (let ((nowrap-lines 0))
            (goto-char (point-min))
            (while (not (eobp))
              (let ((width (string-width (buffer-substring (line-beginning-position) (line-end-position)))))
                (if (get-text-property (point) 'thb-md-render--nowrap)
                    (when (> width columns) (cl-incf nowrap-lines))
                  (should (<= width (1- columns)))))
              (forward-line 1))
            ;; Code stays one long line; the table now fits the window
            ;; (see the fitted-table tests), so only the code line overflows.
            (should (>= nowrap-lines 1))))))))

(ert-deftest thb-md-render-omits-continuation-markers ()
  (thb-md-test--with-render
      "> quote one\n> quote two\n>\n> ```\n> code line\n> ```\n"
    (let ((text (buffer-string)))
      (should-not (string-match-p ">" text))
      (should (string-match-p (regexp-quote "▎ quote one quote two") text))
      (should (string-match-p "^▎   code line$" text)))))

(ert-deftest thb-md-render-keeps-quoted-code-on-one-line ()
  (let ((code (mapconcat #'identity (make-list 40 "code") " ")))
    (thb-md-test--with-render (concat "> ```\n> " code "\n> ```\n")
      (should (string-match-p (concat "^▎   " (regexp-quote code) "$")
                              (buffer-string))))))

(ert-deftest thb-md-render-renders-line-breaks-and-entities ()
  (thb-md-test--with-render
      "one\ntwo\\\nthree  \nfour &amp; &mdash; &#65; &#x42; &bogus;\n"
    (should (string-match-p
             (regexp-quote "one two\nthree\nfour & — A B &bogus;")
             (buffer-string)))))

(ert-deftest thb-md-render-nested-inline-keeps-context-faces ()
  (thb-md-test--with-render "# The *quick* fox\n\n*a **b** c*\n"
    (goto-char (point-min))
    (search-forward "quick")
    (let ((face (get-text-property (1- (point)) 'face)))
      (should (memq 'thb-md-render-emphasis face))
      (should (cl-some (lambda (f)
                         (and (consp f)
                              (eq (plist-get f :inherit) 'thb-md-render-h1)))
                       face)))
    (search-forward "b")
    (let ((face (get-text-property (1- (point)) 'face)))
      (should (memq 'thb-md-render-strong face))
      (should (memq 'thb-md-render-emphasis face)))))

(ert-deftest thb-md-render-link-text-renders-inline-markup ()
  (thb-md-test--with-render "[**bold** `c`](http://x.y)\n"
    (should (string-match-p "^bold c$" (buffer-string)))
    (goto-char (point-min))
    (search-forward "bold")
    (let ((pos (1- (point))))
      (should (memq 'thb-md-render-strong (get-text-property pos 'face)))
      (should (memq 'thb-md-render-link-text (get-text-property pos 'face)))
      (should (equal (get-text-property pos 'help-echo) "http://x.y")))))

(ert-deftest thb-md-render-indents-later-list-item-blocks ()
  (thb-md-test--with-render "- one\n  two\n\n  second\n\n  ```\n  code\n  ```\n"
    (let ((text (buffer-string)))
      (should (string-match-p "^• one two\n\n  second\n" text))
      (should (string-match-p "^    code$" text)))))

(ert-deftest thb-md-render-nests-quote-wrap-prefix ()
  (thb-md-test--with-render "> - item\n"
    (goto-char (point-min))
    (search-forward "item")
    (should (equal (substring-no-properties
                    (get-text-property (1- (point)) 'wrap-prefix))
                   "▎   "))))

(ert-deftest thb-md-render-thematic-break-follows-wrap-width ()
  (thb-md-test--with-render "above\n\n---\n\nbelow\n"
    (save-window-excursion
      (set-window-buffer (selected-window) (current-buffer))
      (dolist (columns '(40 72))
        (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) columns))
                  ((symbol-function 'display-graphic-p) (lambda (&optional _) nil)))
          (thb-md-render--rewrap-cached-content)
          (goto-char (point-min))
          (should (re-search-forward "^─+$" nil t))
          (should (= (- (match-end 0) (match-beginning 0)) (1- columns))))))))

(ert-deftest thb-md-render-watch-retry-backs-off ()
  (with-temp-buffer
    (setq thb-md-render--source-file "/nonexistent/thb-md-test.md")
    (let (delays)
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (secs &rest _) (push secs delays) 'fake-timer))
                ((symbol-function 'timerp) (lambda (_) nil)))
        (dotimes (_ 8)
          (thb-md-render--run-watch-update (current-buffer) t)))
      (should (equal (nreverse delays) '(0.2 0.4 0.8 1.6 3.2 5.0 5.0 5.0))))))

;;; thb-markdown-render-terminal-test.el ends here
