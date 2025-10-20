;;; local.el --- Local configuration (not synced to dotfiles repo) -*- lexical-binding: t; -*-

;; Org-jira configuration (Datadog-specific)
(use-package! org-jira
  :after org
  :config
  (setq jiralib-url "https://datadoghq.atlassian.net"
        org-jira-working-dir "~/Documents/org/jira"
        org-jira-default-jql "assignee = currentUser() AND project IN (AOE, INPLAT, APMS) AND created >= '2025-01-01' order by updated DESC"
        org-jira-use-status-as-todo t
        org-jira-done-states '("DONE" "CANCELLED" "WON'T-DO")
        org-jira-todo-states '("TO-DO" "TODO" "IN-PROGRESS" "BLOCKED" "PAUSED" "ICEBOX")
        ;; Enable debug logging
        org-jira-verbose-logging t)

  ;; Custom function to preserve tags when updating org-jira issues
  (defun thb/org-jira-preserve-tags (orig-fun &rest args)
    "Preserve custom tags when updating org-jira issues."
    (let ((existing-tags (when (org-at-heading-p)
                           (org-get-tags))))
      (apply orig-fun args)
      (when existing-tags
        (org-set-tags (append (org-get-tags)
                              (cl-remove-if (lambda (tag)
                                              (string-match-p "^[A-Z]+-[0-9]+$" tag))
                                            existing-tags))))))

  (advice-add 'org-jira--render-issue :around #'thb/org-jira-preserve-tags)

  ;; Fix smart-link conversion for Jira comments and descriptions
  (defun thb/org-jira-convert-smart-links (text)
    "Convert Jira smart-link syntax to org-mode link syntax in TEXT.
Converts [URL1|URL2|smart-link] to [[URL1][description]]."
    (replace-regexp-in-string
     "\\[\\([^|]+\\)|[^|]*|smart-link\\]"
     "[[\\1][\\1]]"
     text))

  (defun thb/org-jira-insert-with-smart-link-conversion (orig-fun &rest args)
    "Convert smart-links before inserting text in org-jira."
    (let ((converted-args (mapcar (lambda (arg)
                                    (if (stringp arg)
                                        (thb/org-jira-convert-smart-links arg)
                                      arg))
                                  args)))
      (apply orig-fun converted-args)))

  (advice-add 'org-jira-insert :around #'thb/org-jira-insert-with-smart-link-conversion)

  ;; Function to clean up existing smart-links in JIRA files
  (defun thb/org-jira-cleanup-smart-links-in-buffer ()
    "Clean up malformed smart-links in the current buffer."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[\\([^|]+\\)|[^|]*|smart-link\\]" nil t)
        (replace-match "[[\\1][\\1]]"))))

  (defun thb/org-jira-cleanup-all-smart-links ()
    "Clean up malformed smart-links in all JIRA org files."
    (interactive)
    (dolist (file (directory-files org-jira-working-dir t "\\.org$"))
      (with-current-buffer (find-file-noselect file)
        (thb/org-jira-cleanup-smart-links-in-buffer)
        (save-buffer))))

  ;; Ensure the working directory exists
  (make-directory org-jira-working-dir t)

  (map! :leader
        :desc "Get Jira issues" "j g" #'org-jira-get-issues
        :desc "Create Jira issue" "j c" #'org-jira-create-issue
        :desc "Update Jira issue" "j u" #'org-jira-update-issue
        :desc "Get Jira projects" "j p" #'org-jira-get-projects
        :desc "Browse Jira issue" "j b" #'org-jira-browse-issue
        :desc "Progress Jira issue" "j t" #'org-jira-progress-issue
        :desc "Refresh Jira issue" "j r" #'org-jira-refresh-issue))

;; Project-specific configuration for dd-source
(dir-locals-set-class-variables
 'dd-source-project
 '((go-mode . ((lsp-go-build-flags . ["-tags=dynamic"])
               (lsp-go-env . ((GOFLAGS . "-tags=dynamic")))
               (lsp-headerline-breadcrumb-enable . nil)
               (lsp-lens-enable . nil)
               (lsp-signature-auto-activate . nil)
               (lsp-signature-render-documentation . nil)
               (lsp-completion-show-detail . nil)
               (lsp-completion-show-kind . nil)))))

(dir-locals-set-directory-class "~/dd/dd-source" 'dd-source-project)

;; Datadog-specific LSP and tool configuration
(after! lsp-mode
  ;; Add dd-specific bazel directories to ignore list
  (setq lsp-go-directory-filters
        (vconcat lsp-go-directory-filters ["-bazel-dd-go" "-bazel-dd-source"]))

  (setq lsp-file-watch-ignored-directories
        (append lsp-file-watch-ignored-directories
                '("[/\\\\]bazel-dd-go$" "[/\\\\]bazel-dd-source$"))))

(after! projectile
  ;; Add dd-specific bazel directories to projectile ignore list
  (setq projectile-globally-ignored-directories
        (append projectile-globally-ignored-directories
                '("bazel-dd-go" "bazel-dd-source"))))

;; Magit optimizations for dd-source
(after! magit
  ;; Additional optimizations for large repos like dd-source
  (when (and (boundp 'projectile-project-root)
             (string-match-p "dd-source" (or (projectile-project-root) "")))
    (setq-local magit-refresh-verbose nil
                magit-diff-highlight-indentation nil
                magit-diff-paint-whitespace nil))

  ;; Add ~/dd to repository scanning
  (when (boundp 'magit-repository-directories)
    (setq magit-repository-directories
          (append magit-repository-directories '(("~/dd" . 1))))))

;; Project-specific optimizations for dd-source
(after! projectile
  (when (and (fboundp 'projectile-project-root)
             (let ((root (projectile-project-root)))
               (and root (string-match-p "dd-source" root))))
    ;; Additional optimizations for large repositories
    (setq-local lsp-file-watch-threshold 500    ; Lower file watch threshold
                magit-refresh-status-buffer nil  ; Disable status buffer auto-refresh
                vc-follow-symlinks t             ; Don't ask about symlinks
                projectile-enable-caching t)))   ; Force projectile caching
