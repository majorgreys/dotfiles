;;; org-config.el -*- lexical-binding: t; -*-

;;
;; Org-mode and Org-roam configuration
;;

;; ~/org is a symlink to ~/Documents/org
(setq org-directory "~/org/")
(setq org-roam-directory (expand-file-name "roam/" org-directory))
(setq org-roam-dailies-directory (expand-file-name "daily/" org-directory))

;; Org TODO keywords and priorities
(after! org
  ;; Line spacing for org-modern box rendering (calculates borders from this)
  (setq-default line-spacing 0.1)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)")))

  ;; Custom org-babel blocks
  (add-to-list 'org-structure-template-alist '("slack" . "src slack"))
  (add-to-list 'org-structure-template-alist '("quote" . "quote"))

  ;; Auto-generate diagram filenames with timestamp (shared by mermaid, dot, etc.)
  (defun thb/org-babel-diagram-filename ()
    "Generate timestamped diagram filename in diagrams/YYYYMM/ directory.
Returns absolute path for diagram renderers to write file to."
    (let* ((date-dir (format-time-string "%Y%m"))
           (timestamp (format-time-string "%s")))
      (expand-file-name
       (format "diagrams/%s/%s-diagram.png" date-dir timestamp)
       org-directory)))

  ;; Enable mermaid diagram support via ob-mermaid
  (require 'ob-mermaid)
  (setq ob-mermaid-cli-path "mmdc"
        ob-mermaid-output-file-format "png"
        org-babel-default-header-args:mermaid
        '((:results . "file")
          (:mermaid-config-file . "~/org/config/mermaid-config.json")
          (:cmdline . "--theme default --scale 2")))

  ;; Hook into mermaid execution to auto-generate :file
  (advice-add 'org-babel-execute:mermaid :around
              (lambda (orig-fn body params)
                "Execute mermaid block, auto-generating :file if not specified."
                (let* ((auto-file (thb/org-babel-diagram-filename))
                       ;; Build params: add :file if missing, ensure :results file
                       (file (or (cdr (assoc :file params)) auto-file))
                       (params (if (assoc :file params)
                                   params
                                 (cons (cons :file file) params)))
                       (file-dir (file-name-directory file)))
                  (make-directory file-dir t)
                  ;; Call original - it will return nil for :results file to work
                  (funcall orig-fn body params))))

  ;; Display inline images at reasonable pixel width (scaled to fit window)
  (setq org-image-actual-width 1600)

  ;; Auto-display inline images after code block execution
  (defun thb/mermaid-insert-results ()
    "Insert file link in RESULTS block if it's for a mermaid block."
    (save-excursion
      ;; Get current src block info
      (let ((element (org-element-at-point)))
        (when (and element (eq (org-element-type element) 'src-block))
          (let ((lang (org-element-property :language element)))
            (when (string-equal lang "mermaid")
              ;; This is a mermaid block - find the RESULTS block
              (let ((block-end (org-element-property :end element)))
                (goto-char block-end)
                ;; Look for #+RESULTS:
                (when (re-search-forward "^#\\+RESULTS:" (+ block-end 500) t)
                  (let ((result-start (match-end 0)))
                    ;; Check if result is empty (only whitespace until next heading or block)
                    (forward-line 1)
                    (let ((next-content (point)))
                      (if (looking-at "^\\(#\\|$\\|\\*\\)")
                          ;; RESULTS is empty - find the most recent mermaid file
                          (let ((latest-file
                                 (car (last (directory-files
                                             (expand-file-name (format "diagrams/%s" (format-time-string "%Y%m")) org-directory)
                                             t "\\.png$" nil)))))
                            (when latest-file
                              (let ((rel-file (file-relative-name latest-file (file-name-directory (buffer-file-name)))))
                                (goto-char result-start)
                                (insert "\n[[file:" rel-file "]]")))))))))))))))

  (add-hook 'org-babel-after-execute-hook #'thb/mermaid-insert-results)
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images)

  ;; Graphviz (dot) diagram support
  (require 'ob-dot)
  (setq org-babel-default-header-args:dot '((:results . "file")))

  ;; Inject default graph styling into dot code
  (defun thb/org-babel-dot-inject-defaults (body params)
    "Inject default Graphviz styling into dot body."
    (let ((defaults (concat "\n  dpi=200;\n"
                            "  rankdir=LR;\n"
                            "  node [fontname=\"PragmataPro Mono\", fontsize=11, margin=\"0.3,0.15\"];\n"
                            "  edge [fontsize=9];")))
      (replace-regexp-in-string
       "{" (concat "{" defaults)
       body)))

  ;; Pre-process dot blocks before execution: add :file if missing
  (defun thb/dot-add-file-param ()
    "Add :file parameter to dot blocks that don't have one."
    (let ((element (org-element-at-point)))
      (when (and element (eq (org-element-type element) 'src-block))
        (when (string-equal (org-element-property :language element) "dot")
          ;; Check if block already has :file
          (let ((params (org-element-property :parameters element)))
            (unless (string-match ":file" params)
              ;; Block doesn't have :file - add it
              (let* ((file-name (thb/org-babel-diagram-filename))
                     (file-dir (file-name-directory file-name)))
                (make-directory file-dir t)
                ;; Add :file to the header-args
                (save-excursion
                  (goto-char (org-element-property :begin element))
                  (search-forward "#+begin_src dot")
                  (insert (format " :file %s" file-name))))))))))

  (add-hook 'org-babel-before-execute-src-block-hook #'thb/dot-add-file-param)

  ;; Execute dot blocks with styling defaults injected
  (defadvice! thb/org-babel-execute:dot--inject-defaults (orig-fn body params)
    :around #'org-babel-execute:dot
    (funcall orig-fn (thb/org-babel-dot-inject-defaults body params) params))

  ;; D2 diagram support via ob-d2
  (require 'ob-d2)
  (setq org-babel-default-header-args:d2
        '((:results . "file")
          (:exports . "results")
          (:cmdline . "--scale 2 --elk-nodeNodeBetweenLayers 30 --elk-padding \"[top=30,left=30,bottom=30,right=30]\"")))

  ;; Python babel blocks - use uv run for PEP 723 inline dependency support
  ;; The dash tells uv to read script from stdin (not launch interpreter)
  ;; This enables automatic dependency resolution from # /// script blocks
  (setq org-babel-python-command "uv run -"
        org-babel-default-header-args:python
        '((:results . "output")))

  ;; Bash/shell babel blocks - disable colors by default
  ;; This prevents ANSI color escape codes in output (e.g., from ddr commands)
  (let ((shell-args '((:results . "output")
                      (:prologue . "export DDR_DISABLE_COLORS=true"))))
    (setq org-babel-default-header-args:bash shell-args
          org-babel-default-header-args:sh shell-args))

  ;; Timestamp when marking DONE, store in LOGBOOK drawer
  (setq org-log-done 'time
        org-log-into-drawer t)

  ;; Auto-save for org-mode (auto-save-visited-mode writes to real file, not #file#)
  (setq auto-save-visited-interval 10)
  (auto-save-visited-mode 1))

;; Org-roam configuration - load at startup for emacsclient compatibility
(use-package! org-roam
  :demand t  ; Load at startup to ensure org-roam-skill is available via emacsclient
  :config
  (org-roam-db-autosync-mode))

;; Org-roam node sorting and display
(after! org-roam
  (setq org-roam-node-default-sort 'file-mtime  ; Sort by modified time
        ;; Show tags in main completion UI
        org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag))
        ;; Show modified time as annotation
        org-roam-node-annotation-function
        (lambda (node)
          (marginalia--time (org-roam-node-file-mtime node)))))

;; Configure Vertico to preserve org-roam's sorting and enable grid mode
(after! vertico
  (setq vertico-multiform-commands
        '((org-roam-node-find
           (vertico-sort-function . nil)
           vertico-grid-mode)))

  ;; Grid configuration - show more notes at once
  (setq vertico-grid-separator "  "
        vertico-grid-lookahead 50))

;; Embark actions for org-roam nodes (Doom already configures embark)
(after! embark
  (defvar-keymap embark-org-roam-node-map
    :doc "Actions for org-roam nodes"
    :parent embark-general-map
    "o" #'org-roam-node-open
    "i" #'org-roam-node-insert
    "r" #'org-roam-buffer-toggle
    "f" #'org-roam-node-find)

  (add-to-list 'embark-keymap-alist '(org-roam-node . embark-org-roam-node-map))

  ;; Make embark recognize org-roam-node candidates
  (defun embark-org-roam-node-target ()
    "Target org-roam nodes in completion."
    (when-let ((node (get-text-property 0 'node (minibuffer-contents-no-properties))))
      `(org-roam-node ,(org-roam-node-title node) . ,node)))

  (add-to-list 'embark-target-finders 'embark-org-roam-node-target))

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

;; Vulpea — primary note interface (built on org-roam-db)
(use-package! vulpea
  :after org
  :config
  (setq vulpea-db-sync-directories
        (list (expand-file-name "roam/" org-directory)
              (expand-file-name "daily/" org-directory))
        vulpea-default-notes-directory
        (expand-file-name org-directory)
        vulpea-db-parse-method         'single-temp-buffer
        vulpea-db-sync-scan-on-enable  'async
        vulpea-db-sync-external-method 'fswatch
        vulpea-db-index-heading-level  t)

  (defun thb/vulpea-describe-with-mtime (note)
    "Format NOTE as [mtime] title for completion candidates."
    (let* ((path (vulpea-note-path note))
           (attrs (when path (file-attributes path)))
           (mtime (when attrs
                    (file-attribute-modification-time attrs)))
           (mtime-str (if mtime
                          (format-time-string "[%Y-%m-%d %a %H:%M]" mtime)
                        (make-string 22 ?\s)))
           (title (vulpea-note-title note)))
      (concat (propertize mtime-str 'face 'font-lock-comment-face)
              " " title)))

  (setq vulpea-select-describe-fn #'thb/vulpea-describe-with-mtime)

  (setq vulpea-create-default-template
        '(:file-name "roam/%<%Y%m%d%H%M>.org"
          :head "#+date: %<[%Y-%m-%d]>"
          :properties (("CREATED" . "%<[%Y-%m-%d %a %H:%M]>"))
          :tags nil))

  (vulpea-db-autosync-mode 1))

(use-package! vulpea-ui
  :after vulpea)

(use-package! vulpea-journal
  :after (vulpea vulpea-ui)
  :config
  (setq vulpea-journal-default-template
        '(:file-name "daily/%Y-%m-%d.org"
          :title "%Y-%m-%d %A"
          :tags ("daily")
          :properties (("CREATED" . "%<[%Y-%m-%d]>"))))
  (vulpea-journal-setup))

(use-package! consult-vulpea
  :after vulpea
  :config
  (setq consult-vulpea-grep-func #'consult-ripgrep)
  (consult-vulpea-mode 1))

;; Org-modern for visual improvements
(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star 'replace
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((42 . "◦") (43 . "•") (45 . "–"))
        org-modern-keyword nil
        org-modern-todo t))

;; Org-modern label face
(custom-set-faces!
  '(org-modern-label :height 0.85 :width normal :weight regular))

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
        org-attach-directory (expand-file-name "attachments/" org-directory)
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
              (regexp-quote (expand-file-name org-directory))))

;; org-confluence-publish configuration
;; One-way publishing of org files to Confluence Cloud
(use-package! org-confluence-publish
  :after org
  :commands (org-confluence-publish-buffer org-confluence-publish-open-page)
  :config
  ;; Confluence Cloud connection settings
  ;; Reuse existing JIRA_EMAIL and JIRA_API_TOKEN environment variables
  (setq org-confluence-publish-base-url "https://datadoghq.atlassian.net"
        org-confluence-publish-email (getenv "JIRA_EMAIL")
        org-confluence-publish-api-token (getenv "JIRA_API_TOKEN")
        org-confluence-publish-space-key "~972692212"  ; Personal space
        org-confluence-publish-parent-id "5915312686")  ; DRAFTS page

  ;; Keybindings under SPC m e c (org-mode export confluence)
  (map! :map org-mode-map
        :localleader
        (:prefix ("e" . "export")
                 (:prefix ("c" . "confluence")
                  :desc "Publish to Confluence" "c" #'org-confluence-publish-buffer
                  :desc "Open in browser" "o" #'org-confluence-publish-open-page))))

(provide 'org-config)
