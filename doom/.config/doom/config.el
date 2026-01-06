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

;; Load configuration modules
(load! "org-config")
(load! "dev-config")
(load! "performance-config")
(load! "custom-functions")

;; Load local configs (not synced to repo)
(load! "local" nil t)

;; Custom keybindings
(map! :leader
      :desc "Org-roam insert link" "n r i" #'org-roam-node-insert
      :desc "Org-roam capture" "n r c" #'org-roam-capture
      :desc "Org-roam dailies capture today" "n r d c" #'(lambda () (interactive) (org-roam-dailies-capture-today nil "d"))
      :desc "Org-roam dailies goto today" "n r d d" #'org-roam-dailies-goto-today
      :desc "Org-roam dailies goto yesterday" "n r d y" #'org-roam-dailies-goto-yesterday
      :desc "Consult org-roam" "n r s" #'consult-org-roam-search
      :desc "Yank file path" "y f" #'(lambda () (interactive) (kill-new (buffer-file-name)))
      :desc "Add TODO note" "o n" #'org-add-note
      :desc "Capture TODO to inbox" "c t" #'(lambda () (interactive) (org-capture nil "t"))
      ;; Org-attach keybindings
      :desc "Attach file" "o a a" #'org-attach-attach
      :desc "Open attachment dir" "o a d" #'org-attach-dired
      :desc "Open attachment" "o a o" #'org-attach-open
      :desc "Delete attachment" "o a D" #'org-attach-delete-one
      :desc "Sync attachments" "o a s" #'org-attach-sync
      ;; Claude Code IDE keybindings
      :desc "Claude Code IDE menu" "C m" #'claude-code-ide-menu
      :desc "Start Claude Code IDE" "C s" #'claude-code-ide
      ;; Git auto-commit toggle
      :desc "Toggle git auto-commit" "t g" #'git-auto-commit-mode
      ;; Theme toggle
      :desc "Toggle light/dark theme" "t t" #'thb/toggle-modus-theme
      ;; Transcript processing
      :desc "Process transcript with Claude" "o t" #'thb/process-transcript-file
      :desc "Process current file as transcript" "o T" #'thb/process-current-file-as-transcript)

;; Image mode configuration
(add-hook 'image-mode-hook
  (lambda () (define-key image-mode-map (kbd "f") 'image-transform-fit-to-window)))

(provide 'config)
