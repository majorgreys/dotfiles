;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Set TERMINFO for Ghostty terminal support
;; This allows emacsclient to work properly with Ghostty's xterm-ghostty terminal type
(setenv "TERMINFO" "/Applications/Ghostty.app/Contents/Resources/terminfo")
(setq shell-file-name (executable-find "bash"))

(setq user-mail-address "tahir@tahirbutt.com"
      user-full-name    "Tahir H. Butt"
      doom-theme 'modus-operandi
      doom-font (font-spec :family "PragmataPro Mono Liga" :size 16)
      doom-unicode-font (font-spec :family "PragmataPro Mono Liga" :size 16)
      ;; doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 16)
      doom-big-font (font-spec :family "PragmataPro Mono" :size 24)
      display-line-numbers-type t)

;; Toggle between modus light and dark themes
(defun thb/toggle-modus-theme ()
  "Toggle between modus-operandi (light) and modus-vivendi (dark)."
  (interactive)
  (if (eq doom-theme 'modus-operandi)
      (progn
        (setq doom-theme 'modus-vivendi)
        (load-theme 'modus-vivendi t)
        (message "Dark theme"))
    (setq doom-theme 'modus-operandi)
    (load-theme 'modus-operandi t)
    (message "Light theme")))

;; Load local configs first (machine-specific settings, not synced to repo)
(load! "local" nil t)

;; Load configuration modules
(load! "org-config")
(load! "dev-config")
(load! "performance-config")
;; Custom keybindings
(map! :leader
      :desc "Org-roam insert link" "n r i" #'org-roam-node-insert
      :desc "Org-roam capture" "n r c" #'org-roam-capture
      :desc "Org-roam dailies capture today" "n r d c" #'(lambda () (interactive) (org-roam-dailies-capture-today nil "d"))
      :desc "Org-roam dailies goto today" "n r d d" #'org-roam-dailies-goto-today
      :desc "Org-roam dailies goto yesterday" "n r d y" #'org-roam-dailies-goto-yesterday
      :desc "Consult org-roam" "n r s" #'consult-org-roam-search
      ;; Vulpea keybindings
      :desc "Vulpea find" "n v f" #'vulpea-find
      :desc "Vulpea insert" "n v i" #'vulpea-insert
      :desc "Vulpea backlinks" "n v b" #'vulpea-find-backlink
      :desc "Vulpea search" "n v s" #'consult-vulpea-ripgrep
      :desc "Vulpea journal" "n v j" #'vulpea-journal
      :desc "Vulpea journal date" "n v d" #'vulpea-journal-date
      :desc "Yank file path" "y f" #'(lambda () (interactive) (kill-new (buffer-file-name)))
      ;; Org-attach keybindings
      :desc "Attach file" "o a a" #'org-attach-attach
      :desc "Open attachment dir" "o a d" #'org-attach-dired
      :desc "Open attachment" "o a o" #'org-attach-open
      :desc "Delete attachment" "o a D" #'org-attach-delete-one
      :desc "Sync attachments" "o a s" #'org-attach-sync
      ;; Agent Shell keybindings
      :desc "Agent Shell" "C a" #'agent-shell
      :desc "Agent Shell (Claude Code)" "C c" #'agent-shell-anthropic-start-claude-code
      ;; Theme toggle
      :desc "Toggle light/dark theme" "t t" #'thb/toggle-modus-theme)

;; Agent Shell configuration
(use-package! agent-shell
  :commands (agent-shell agent-shell-anthropic-start-claude-code)
  :config
  (setq agent-shell-anthropic-claude-command nil
        agent-shell-anthropic-claude-acp-command '("claude-agent-acp")
        agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables :inherit-env t)
        agent-shell-prefer-viewport-interaction t)

  ;; Evil bindings for viewport
  (evil-set-initial-state 'agent-shell-viewport-view-mode 'normal)
  (evil-set-initial-state 'agent-shell-viewport-edit-mode 'insert)
  (evil-define-key 'normal agent-shell-viewport-view-mode-map
    "n" #'agent-shell-viewport-next-item
    "p" #'agent-shell-viewport-previous-item
    "f" #'agent-shell-viewport-next-page
    "b" #'agent-shell-viewport-previous-page
    "r" #'agent-shell-viewport-reply
    "y" #'agent-shell-viewport-reply-yes
    "m" #'agent-shell-viewport-reply-more
    "s" #'agent-shell-viewport-set-session-mode
    "q" #'bury-buffer
    "o" #'agent-shell-other-buffer)

  ;; Add f/b page navigation hints to viewport view mode header.
  (define-advice agent-shell-viewport--update-header (:around (orig-fn) page-hints)
    (cl-letf* ((real-make-header (symbol-function 'agent-shell--make-header))
               ((symbol-function 'agent-shell--make-header)
                (lambda (state &rest args)
                  (when (derived-mode-p 'agent-shell-viewport-view-mode)
                    (when-let ((bindings (plist-get args :bindings)))
                      (plist-put args :bindings
                                 (append bindings
                                         (list '((:key . "f") (:description . "forward"))
                                               '((:key . "b") (:description . "back")))))))
                  (apply real-make-header state args))))
      (funcall orig-fn)))

  ;; Word wrap and visual distinction for viewport buffers.
  (defun thb/agent-shell-viewport-view-setup ()
    (setq-local truncate-lines t))
  (defun thb/agent-shell-viewport-edit-setup ()
    (visual-line-mode 1)
    (setq-local word-wrap t))
  (add-hook 'agent-shell-viewport-view-mode-hook #'thb/agent-shell-viewport-view-setup)
  (add-hook 'agent-shell-viewport-edit-mode-hook #'thb/agent-shell-viewport-edit-setup)

  ;; Make prompt visually distinct: background highlight + separator.
  (define-advice agent-shell-viewport--initialize (:after (&rest _) style-prompt)
    (when (derived-mode-p 'agent-shell-viewport-view-mode)
      (save-excursion
        (goto-char (point-min))
        (when-let* ((start (or (and (get-text-property (point-min) 'agent-shell-viewport-prompt)
                                    (point-min))
                               (next-single-property-change (point-min) 'agent-shell-viewport-prompt)))
                    (end (next-single-property-change start 'agent-shell-viewport-prompt)))
          (let ((inhibit-read-only t))
            (add-face-text-property start end
                                    `(:background ,(modus-themes-get-color-value 'bg-blue-nuanced) :extend t))
            (goto-char end)
            (insert (propertize (concat (make-string 60 ?─) "\n")
                                'face `(:foreground ,(modus-themes-get-color-value 'border))
                                'read-only t
                                'rear-nonsticky t)))))))

  ;; Fit markdown tables to window width with word-wrapped cells.
  (defun thb/md-table-render-inline (str)
    "Render markdown inline formatting in STR. Return propertized string."
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward "\\*\\*\\([^*]+\\)\\*\\*" nil t)
        (replace-match (propertize (match-string 1) 'face 'bold)))
      (goto-char (point-min))
      (while (re-search-forward "`\\([^`]+\\)`" nil t)
        (replace-match (propertize (match-string 1) 'face 'markdown-inline-code-face)))
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

  (define-advice agent-shell-viewport--initialize (:after (&rest _) align-tables)
    (when (derived-mode-p 'agent-shell-viewport-view-mode)
      (save-excursion
        (goto-char (point-min))
        (let ((inhibit-read-only t)
              (win-width (- (or (when-let ((win (get-buffer-window (current-buffer))))
                                  (window-body-width win))
                                (window-body-width))
                             2)))
          (while (re-search-forward "^[ \t]*|[^|\n]+|" nil t)
            (let ((tbl-start (line-beginning-position)))
              (forward-line)
              (while (and (not (eobp))
                          (looking-at "^[ \t]*|[^|\n]*|"))
                (forward-line))
              (let* ((tbl-end (point))
                     (tbl-text (buffer-substring-no-properties tbl-start tbl-end))
                     (parsed (ignore-errors (thb/md-table-parse tbl-text))))
                (when (and parsed (car parsed) (cdr parsed)
                           (>= (length (car parsed)) 2))
                  (let ((formatted (ignore-errors
                                     (thb/md-table-format tbl-text win-width))))
                    (when formatted
                      (delete-region tbl-start tbl-end)
                      (goto-char tbl-start)
                      (insert formatted "\n"))))))))))))

;; Image mode configuration
(add-hook 'image-mode-hook
  (lambda () (define-key image-mode-map (kbd "f") 'image-transform-fit-to-window)))

(provide 'config)
