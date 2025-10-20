;;; org-config.el -*- lexical-binding: t; -*-

;;
;; Org-mode and Org-roam configuration
;;

;; Set org directories
(setq org-directory "~/Documents/org/"
      org-roam-directory "~/Documents/org/roam/"
      org-roam-dailies-directory "~/Documents/org/daily/")

;; Org-agenda configuration
(after! org-agenda
  (setq org-agenda-files '("~/Documents/org/daily/"
                           "~/Documents/org/roam/"
                           "~/Documents/org/projects/"
                           ;; "~/Documents/org/jira/"
                           ;; "~/Documents/org/jira/AOE.org"
                           ;; "~/Documents/org/jira/INPLAT.org"
                           "~/Documents/org/todo.org")
        org-agenda-start-day nil
        org-agenda-start-on-weekday nil
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t))

;; Org TODO keywords and priorities
(after! org
  (setq org-todo-keywords
        '((sequence "ICEBOX(I) BACKLOG(b)" "TO-DO(T)" "TODO(t)" "IN-PROGRESS(i)" "PAUSED(p)" "BLOCKED(B)" "STALE(s)" "|" "DONE(d)" "CANCELLED(c)" "WON'T-DO(w)"))
        org-priority-highest ?A
        org-priority-lowest ?C
        org-priority-default ?B)

  ;; Custom org-babel blocks
  (add-to-list 'org-structure-template-alist '("slack" . "src slack"))
  (add-to-list 'org-structure-template-alist '("quote" . "quote"))

  ;; TODO timestamp tracking
  (setq org-log-done 'time                              ; Timestamp when marking DONE
        org-log-into-drawer t                           ; Store in LOGBOOK drawer
        org-treat-insert-todo-heading-as-state-change t ; Log TODO creation
        org-log-repeat 'time                            ; Log repeating tasks
        org-log-reschedule 'time                        ; Log schedule changes
        org-log-redeadline 'time)                       ; Log deadline changes

  ;; Prevent TAB from cycling TODO states - only handle folding
  (setq org-cycle-include-plain-lists nil)
  (advice-add 'org-cycle :before-while
              (lambda (&rest _) (not (org-at-heading-p))))

  ;; Auto-save configuration for org-mode
  (add-hook 'org-mode-hook 'auto-save-mode)
  (setq auto-save-interval 20                    ; Save every 20 characters
        auto-save-timeout 10                     ; Save after 10 seconds of idle time
        auto-save-default t)

  ;; Org-specific auto-save (recommended approach)
  (add-hook 'org-mode-hook #'auto-save-mode)
  (setq auto-save-visited-interval 10            ; Auto-save every 10 seconds
        auto-save-visited-mode t))

;; Org-capture templates
(after! org
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Documents/org/todo.org" "Inbox")
           "** TODO %?\nSCHEDULED: %t\n")
          ("i" "Interrupt" entry (file+headline "~/Documents/org/todo.org" "Inbox")
           "** TODO %?:interrupt:\nSCHEDULED: %t\n")
          ("p" "Project Task" entry (file+headline "~/Documents/org/todo.org" "Projects")
           "*** TODO %?\nSCHEDULED: %t\n")
          ("r" "Research" entry (file+headline "~/Documents/org/todo.org" "Inbox")
           "** TODO %? :research:\n"))))

;; Org-super-agenda configuration
(use-package! org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode 1)
  (setq org-super-agenda-groups
        '((:name "Today"
           :time-grid t
           :scheduled today
           :deadline today)
          (:name "Overdue"
           :deadline past
           :scheduled past)
          (:name "High Priority"
           :priority "A")
          (:name "In Progress"
           :todo "IN-PROGRESS")
          (:name "Waiting"
           :todo "WAITING")
          (:name "Projects"
           :tag "project")
          (:name "Work"
           :tag "work")
          (:name "Low Priority"
           :priority "C")
          (:name "Someday"
           :tag "someday"))))

;; Consult-org-roam for enhanced search
(use-package! consult-org-roam
  :after org-roam
  :config
  (setq consult-org-roam-grep-func #'consult-ripgrep))

;; Org-roam node sorting
(after! org-roam
  (setq org-roam-node-default-sort 'file-atime)  ; Sort by access time

  ;; Disable Vertico sorting for org-roam-node-find to preserve org-roam's sorting
  (defun my/org-roam-node-find ()
    "Find org-roam node with proper sorting."
    (interactive)
    (let ((vertico-sort-function nil))
      (org-roam-node-find)))

  ;; Override default binding
  (map! :leader
        :desc "Find node (sorted)" "n r f" #'my/org-roam-node-find))

;; Org-roam capture templates
(after! org-roam
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M>.org"
                              "#+title: ${title}\n#+date: %<%Y-%m-%d>\n#+created: %U\n\n")
           :unnarrowed t)
          ("n" "named note" plain "%?"
           :target (file+head "${slug}.org"
                              "#+title: ${title}\n#+date: %<%Y-%m-%d>\n\n")
           :unnarrowed t)))

  ;; Org-roam dailies capture templates
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n"))
          ("p" "priorities" entry "* Today's priorities\n:PROPERTIES:\n:CREATED: %U\n:END:\n1. %?\n2. \n3. \n"
           :target (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n#+filetags: :daily:\n\n")
           :unnarrowed t
           :empty-lines 1
           :kill-buffer t))))

;; Org-modern for visual improvements
(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star 'replace
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((42 . "◦") (43 . "•") (45 . "–"))
        org-modern-todo-faces
        '(("TODO" :inverse-video t :inherit org-todo)
          ("IN-PROGRESS" :inverse-video t :inherit org-todo)
          ("WAITING" :inverse-video t :inherit org-todo)
          ("DONE" :inverse-video t :inherit org-done)
          ("CANCELLED" :inverse-video t :inherit org-done))))

;; Org-tidy to hide property drawers and show visual indicators
(use-package! org-tidy
  :after org
  :hook (org-mode . org-tidy-mode)
  :config
  (setq org-tidy-properties-style 'inline        ; Show inline symbol
        org-tidy-top-property-style 'invisible   ; Keep top-level properties visible
        org-tidy-protect-fontification nil))     ; Allow editing in property regions

;; Org-attach configuration for date-based file organization
(after! org
  ;; Use timestamp-based IDs for date-based organization
  (setq org-id-method 'ts                        ; Use timestamp IDs
        org-attach-id-to-path-function-list
        '(org-attach-id-ts-folder-format          ; Date-based folder format
          org-attach-id-uuid-folder-format)       ; Fallback to UUID format
        org-attach-directory "~/Documents/org/attachments/"
        org-attach-method 'cp                    ; Copy files (safer than move)
        org-attach-store-link-p 'attached        ; Store link after attaching files
        org-attach-use-inheritance t             ; Inherit attachment settings
        org-attach-dir-relative t                ; Use relative paths in links
        org-attach-preferred-new-method 'dir     ; Create directories for attachments
        org-attach-archive-delete 'query))       ; Ask before deleting on archive

;; Org export with pandoc support
(after! ox-pandoc
  (setq org-pandoc-options-for-docx '((standalone . t))
        org-pandoc-options-for-latex-pdf '((pdf-engine . "xelatex"))
        org-pandoc-options '((standalone . t)
                             (self-contained . t))))

;; Additional export formats via ox-gfm (GitHub Flavored Markdown)
(after! ox-gfm
  (eval-after-load "org"
    '(require 'ox-gfm nil t)))

;; Zen mode configuration - disable variable pitch fonts
(after! mixed-pitch
  (setq +zen-mixed-pitch-modes '()))

;; Disable line numbers in zen mode
(after! writeroom-mode
  (setq writeroom-local-effects '(display-line-numbers-mode)))

;; Exclude org directory from version control to prevent git errors
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              (regexp-quote (expand-file-name "~/Documents/org"))))

;; Git auto-commit configuration for org files
(use-package! git-auto-commit-mode
  :config
  ;; Set commit message format
  (setq gac-automatically-push-p nil  ; Don't auto-push to remote
        gac-automatically-add-new-files-p t  ; Auto-add new files
        gac-debounce-interval 10)  ; Wait 10 seconds before committing

  ;; Custom commit message function - fix the lambda
  (setq gac-default-message
        (lambda (_)
          (let ((filename (file-name-nondirectory (buffer-file-name))))
            (format "Update %s" filename)))))

;; Enable git-auto-commit-mode for org files in org directory
(add-hook 'org-mode-hook
          (lambda ()
            (when (and (buffer-file-name)
                       (string-prefix-p (expand-file-name org-directory)
                                        (buffer-file-name)))
              (git-auto-commit-mode 1))))

(provide 'org-config)
