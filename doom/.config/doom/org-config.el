;;; org-config.el -*- lexical-binding: t; -*-

;;
;; Org-mode and Org-roam configuration
;;

;; Set org directories (can be overridden in local.el which loads first)
(setq org-directory (if (boundp 'org-directory) org-directory "~/Documents/org/"))
(setq org-roam-directory (expand-file-name "roam/" org-directory))
(setq org-roam-dailies-directory (expand-file-name "daily/" org-directory))

;; Org-agenda configuration
(after! org-agenda
  ;; Primary task source - todo.org contains canonical task state
  ;; Daily/roam/weekly notes provide context but are not agenda sources
  ;; Use org-roam-find (SPC n r f) to search for narrative context
  (setq org-agenda-files (list (expand-file-name "todo.org" org-directory))
        org-agenda-start-day nil
        org-agenda-start-on-weekday nil
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t)

  ;; Auto-save todo.org after any agenda edit operation
  ;; This ensures changes made in agenda view are immediately persisted
  ;; Note: Must use lambda to ignore arguments passed by :after advice
  (advice-add 'org-agenda-todo :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-agenda-priority :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-agenda-schedule :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-agenda-deadline :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-agenda-set-tags :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-agenda-archive-default :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-agenda-kill :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-agenda-refile :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-agenda-bulk-action :after (lambda (&rest _) (org-save-all-org-buffers))))

;; Org TODO keywords and priorities
(after! org
  ;; Line spacing for better org-modern box rendering
  ;; org-modern calculates box borders from line-spacing
  ;; Recommended: 0.1-0.3 for proper vertical centering
  ;; Phase 4: Using minimum recommended value for tighter boxes
  (setq-default line-spacing 0.1)

  (setq org-todo-keywords
        '((sequence "ICEBOX(I) BACKLOG(b)" "TO-DO(T)" "TODO(t)" "IN-PROGRESS(i)" "PAUSED(p)" "BLOCKED(B)" "|" "DONE(d)" "CANCELLED(c)"))
        org-priority-highest ?A
        org-priority-lowest ?C
        org-priority-default ?B)

  ;; Custom org-babel blocks
  (add-to-list 'org-structure-template-alist '("slack" . "src slack"))
  (add-to-list 'org-structure-template-alist '("quote" . "quote"))

  ;; Enable mermaid diagram support via ob-mermaid
  (require 'ob-mermaid)
  (add-to-list 'org-babel-load-languages '(mermaid . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (setq ob-mermaid-cli-path "mmdc"
        ob-mermaid-output-file-format "png"
        org-babel-default-header-args:mermaid
        '((:results . "file")
          (:mermaid-config-file . "~/Documents/org/config/mermaid-config.json")
          (:cmdline . "--theme default --scale 2")))

  ;; Auto-generate mermaid filenames with timestamp
  (defun thb/org-babel-mermaid-filename ()
    "Generate timestamped mermaid filename in diagrams/YYYYMM/ directory.
Returns absolute path for mmdc to write file to."
    (let* ((date-dir (format-time-string "%Y%m"))
           (timestamp (format-time-string "%s")))
      (expand-file-name
       (format "diagrams/%s/%s-diagram.png" date-dir timestamp)
       org-directory)))

  ;; Hook into mermaid execution to auto-generate :file
  (advice-add 'org-babel-execute:mermaid :around
              (lambda (orig-fn body params)
                "Execute mermaid block, auto-generating :file if not specified."
                (let* ((auto-file (thb/org-babel-mermaid-filename))
                       ;; Build params: add :file if missing, ensure :results file
                       (file (or (cdr (assoc :file params)) auto-file))
                       (params (if (assoc :file params)
                                   params
                                 (cons (cons :file file) params)))
                       (file-dir (file-name-directory file)))
                  ;; Ensure output directory exists
                  (unless (file-exists-p file-dir)
                    (make-directory file-dir t))
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
                                             (expand-file-name "diagrams/202511" org-directory)
                                             t "\\.png$" nil)))))
                            (when latest-file
                              (let ((rel-file (file-relative-name latest-file (file-name-directory (buffer-file-name)))))
                                (goto-char result-start)
                                (insert "\n[[file:" rel-file "]]")))))))))))))))

  (add-hook 'org-babel-after-execute-hook #'thb/mermaid-insert-results)
  (add-hook 'org-babel-after-execute-hook #'org-display-inline-images)

  ;; Graphviz (dot) diagram support
  (require 'ob-dot)
  (add-to-list 'org-babel-load-languages '(dot . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
  (setq org-babel-default-header-args:dot '((:results . "file")))

  ;; Auto-generate dot filenames with timestamp
  (defun thb/org-babel-dot-filename ()
    "Generate timestamped dot filename in diagrams/YYYYMM/ directory."
    (let* ((date-dir (format-time-string "%Y%m"))
           (timestamp (format-time-string "%s")))
      (expand-file-name
       (format "diagrams/%s/%s-diagram.png" date-dir timestamp)
       org-directory)))

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
              (let ((file-name (thb/org-babel-dot-filename))
                    (file-dir (file-name-directory (thb/org-babel-dot-filename))))
                ;; Create directory
                (unless (file-exists-p file-dir)
                  (make-directory file-dir t))
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
  (add-to-list 'org-babel-load-languages '(d2 . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
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
  (setq org-babel-default-header-args:bash
        '((:results . "output")
          (:prologue . "export DDR_DISABLE_COLORS=true")))
  (setq org-babel-default-header-args:sh
        '((:results . "output")
          (:prologue . "export DDR_DISABLE_COLORS=true")))

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
        auto-save-visited-mode t)

  ;; Enable org-depend for task dependency management
  ;; Provides :BLOCKER: property to establish blocking relationships between tasks
  (require 'org-depend))

;; Org-capture templates
(after! org
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Documents/org/todo.org" "Inbox")
           "** TODO %?\nSCHEDULED: %t\n")
          ("i" "Interrupt" entry (file+olp "~/Documents/org/todo.org" "Areas" "Interrupt")
           "*** TODO %? :interrupt:\nSCHEDULED: %t\n")
          ("e" "Enablement" entry (file+olp "~/Documents/org/todo.org" "Areas" "Enablement")
           "*** TODO %? :enablement:\nSCHEDULED: %t\n")
          ("c" "Compliance" entry (file+olp "~/Documents/org/todo.org" "Areas" "Compliance")
           "*** TODO %? :compliance:\nSCHEDULED: %t\n")
          ("l" "Leadership" entry (file+olp "~/Documents/org/todo.org" "Areas" "Leadership")
           "*** TODO %? :leadership:\nSCHEDULED: %t\n")
          ("p" "Personal" entry (file+olp "~/Documents/org/todo.org" "Areas" "Personal")
           "*** TODO %? :personal:\nSCHEDULED: %t\n")
          ("r" "Research" entry (file+headline "~/Documents/org/todo.org" "Inbox")
           "** TODO %? :research:\n"))))

;; Org-super-agenda configuration
;; Aligned with Daily Planning Protocol v3.0 (Eisenhower Matrix)
(use-package! org-super-agenda
  :after org-agenda
  :config
  (org-super-agenda-mode 1)

  ;; Define preset group configurations for different views
  (setq thb/org-super-agenda-groups-focus
        '(;; Hide PROJ headings and BACKLOG items in focus view
          (:discard (:todo ("PROJ" "BACKLOG" "CANCELLED")))

          ;; P0: Urgent + Important (Oncall, production issues, security)
          (:name "Urgent"
           :tag "oncall"
           :priority "A")

          ;; BLOCKED items need attention to unblock
          (:name "Blocked"
           :todo "BLOCKED")

          ;; Currently active work
          (:name "In progress"
           :todo "IN-PROGRESS"
           :order 1)

          ;; Overdue items that need rescheduling or completion
          (:name "Overdue"
           :and (:scheduled past
                 :todo ("TODO" "PAUSED"))
           :order 2)

          ;; Today's scheduled work
          (:name "Today"
           :and (:scheduled today
                 :todo "TODO")
           :order 3)

          ;; Deadlines within next 3 days
          (:name "Deadlines"
           :deadline past
           :deadline today
           :deadline future
           :order 4)

          ;; Hide future items and deferred work from focus view
          (:discard (:scheduled future))
          (:discard (:todo "PAUSED"))

          ;; Catch remaining actionable items
          (:name "Other"
           :todo "TODO"
           :order 5)))

  (setq thb/org-super-agenda-groups-full
        '(;; P0: Urgent + Important (Oncall, production issues, security)
          (:name "Urgent"
           :tag "oncall"
           :priority "A")

          ;; BLOCKED items (can be any priority - show them prominently)
          (:name "Blocked"
           :todo "BLOCKED")

          ;; P1: Important, Not Urgent (IN-PROGRESS scheduled today/overdue, high completion %)
          (:name "In progress (today)"
           :and (:todo "IN-PROGRESS"
                 :scheduled today))
          (:name "In progress (overdue)"
           :and (:todo "IN-PROGRESS"
                 :scheduled past))

          ;; P2: Committed (TODO items scheduled today with clear next actions)
          (:name "Today"
           :and (:todo "TODO"
                 :scheduled today))

          ;; Deadlines approaching (within 3 days)
          (:name "Deadlines"
           :deadline future
           :deadline today)

          ;; DEFERRED/PAUSED items (truly deferred work)
          (:name "Paused"
           :todo "PAUSED")

          ;; P3: Nice-to-have (Carry-over work, code reviews)
          (:name "Overdue"
           :and (:scheduled past
                 :not (:todo "IN-PROGRESS")))

          ;; Future scheduled items
          (:name "Future"
           :scheduled future)

          ;; Everything else
          (:name "Other"
           :anything t)))

  ;; Set default to focus view
  (setq org-super-agenda-groups thb/org-super-agenda-groups-focus))

;; Custom agenda commands for different views
(after! org-agenda
  (setq org-agenda-custom-commands
        '(("f" "Today's Focus"
           ((agenda ""
                    ((org-agenda-span 1)
                     (org-agenda-start-day nil)
                     (org-super-agenda-groups thb/org-super-agenda-groups-focus)
                     (org-agenda-overriding-header "Today\n"))))
           ((org-agenda-compact-blocks t)
            (org-agenda-skip-deadline-prewarning-if-scheduled t)
            (org-agenda-skip-scheduled-if-deadline-is-shown t)))

          ("a" "Full Agenda"
           ((agenda ""
                    ((org-agenda-span 7)
                     (org-agenda-start-day nil)
                     (org-super-agenda-groups thb/org-super-agenda-groups-full)
                     (org-agenda-overriding-header "Agenda\n"))))
           ((org-agenda-compact-blocks t)))

          ("b" "Blocked Items"
           ((todo "BLOCKED"
                  ((org-agenda-overriding-header "Blocked\n")
                   (org-agenda-sorting-strategy '(priority-down category-keep)))))
           ((org-agenda-compact-blocks t)))

          ("p" "Projects Overview"
           ((tags "PROJ"
                  ((org-agenda-overriding-header "Projects\n")
                   (org-super-agenda-groups
                    '((:auto-property "PROJ_LOCATION")
                      (:discard (:todo "DONE"))
                      (:discard (:todo "CANCELLED")))))))
           ((org-agenda-compact-blocks t)))

          ("i" "In Progress"
           ((todo "IN-PROGRESS"
                  ((org-agenda-overriding-header "In Progress\n")
                   (org-agenda-sorting-strategy '(priority-down scheduled-up)))))
           ((org-agenda-compact-blocks t)))

          ("w" "Weekly Review"
           ((agenda ""
                    ((org-agenda-span 'week)
                     (org-agenda-start-on-weekday 1)
                     (org-super-agenda-groups thb/org-super-agenda-groups-full)
                     (org-agenda-overriding-header "Weekly\n"))))
           ((org-agenda-compact-blocks t))))))

;; Toggle between focus and full agenda views
(defun thb/org-agenda-toggle-view ()
  "Toggle between focus and full agenda views."
  (interactive)
  (if (equal org-super-agenda-groups thb/org-super-agenda-groups-focus)
      (progn
        (setq org-super-agenda-groups thb/org-super-agenda-groups-full)
        (message "Full view"))
    (setq org-super-agenda-groups thb/org-super-agenda-groups-focus)
    (message "Focus view"))
  (when (bound-and-true-p org-agenda-buffer-name)
    (org-agenda-redo-all)))

;; Keybinding for toggle (accessible from agenda view)
(after! org-agenda
  (map! :map org-agenda-mode-map
        "v" #'thb/org-agenda-toggle-view))

;; Consult-org-roam for enhanced search
(use-package! consult-org-roam
  :after org-roam
  :config
  (setq consult-org-roam-grep-func #'consult-ripgrep))

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

;; Org-modern for visual improvements
(use-package! org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star 'replace
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((42 . "◦") (43 . "•") (45 . "–"))
        ;; Phase 4: Let org-modern calculate border automatically from line-spacing
        org-modern-keyword nil     ; Keep keywords disabled for now
        org-modern-todo t))        ; Re-enable TODO box styling

;; Phase 4: Smaller text with normal width, tighter line-spacing
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
;; (use-package! git-auto-commit-mode
;;   :config
;;   ;; Set commit message format
;;   (setq gac-automatically-push-p nil  ; Don't auto-push to remote
;;         gac-automatically-add-new-files-p t  ; Auto-add new files
;;         gac-debounce-interval 10)  ; Wait 10 seconds before committing

;; Custom commit message function - fix the lambda
;; (setq gac-default-message
;;       (lambda (_)
;;         (let ((filename (file-name-nondirectory (buffer-file-name))))
;;           (format "Update %s" filename)))))

;; ;; Enable git-auto-commit-mode for org files in org directory
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (when (and (buffer-file-name)
;;                        (string-prefix-p (expand-file-name org-directory)
;;                                         (buffer-file-name)))
;;               (git-auto-commit-mode 1))))

;; org-db-v3 server process management
;; Automatically start and manage the org-db-v3 FastAPI backend server
(defvar org-db-v3-server-process nil
  "Process object for the org-db-v3 server.")

(defun org-db-v3-start-server ()
  "Start the org-db-v3 FastAPI server if not already running."
  (interactive)
  (let ((repo-path (expand-file-name "~/.config/emacs/.local/straight/repos/org-db-v3/python"))
        (default-directory (expand-file-name "~/.config/emacs/.local/straight/repos/org-db-v3/python")))
    (unless (and org-db-v3-server-process
                 (process-live-p org-db-v3-server-process))
      (setq org-db-v3-server-process
            (make-process
             :name "org-db-v3-server"
             :buffer "*org-db-v3-server*"
             :command `("uv" "run" "uvicorn" "org_db_server.main:app"
                        "--host" "127.0.0.1" "--port" "8765")
             :noquery t
             :stderr "*org-db-v3-server*"))
      (message "Starting org-db-v3 server... (this may take a few seconds on first run)"))))

(defun org-db-v3-stop-server ()
  "Stop the org-db-v3 FastAPI server."
  (interactive)
  (when (and org-db-v3-server-process
             (process-live-p org-db-v3-server-process))
    (delete-process org-db-v3-server-process)
    (setq org-db-v3-server-process nil)
    (message "org-db-v3 server stopped")))

;; Start server automatically when org-db-v3 is first loaded
(add-hook 'emacs-startup-hook
          (lambda ()
            (when (locate-library "org-db-v3")
              (run-at-time 2 nil #'org-db-v3-start-server))))

;; Clean up server on Emacs exit
(add-hook 'kill-emacs-hook #'org-db-v3-stop-server)

;; org-db-v3 configuration
;; Semantic search indexing for org files
(use-package! org-db-v3
  :commands (org-db-v3-semantic-search org-db-v3-index-all org-db-v3-build-db)
  :config
  ;; Index all org files in org directory
  (setq org-db-v3-root-dir org-directory
        org-db-v3-db-file (expand-file-name ".org-db-v3.db" org-directory)

        ;; FastAPI backend configuration
        ;; The backend runs locally and handles embeddings, indexing, and vector search
        org-db-v3-server-port 8765
        org-db-v3-server-host "127.0.0.1"

        ;; Indexing options
        org-db-v3-index-html t           ; Index HTML files
        org-db-v3-index-markdown t       ; Index markdown files
        org-db-v3-index-pdfs nil         ; Don't index PDFs (slower)
        org-db-v3-max-file-size 10485760 ; 10MB max file size

        ;; Search options
        org-db-v3-semantic-similarity-threshold 0.5  ; Relevance threshold
        org-db-v3-semantic-search-limit 20)          ; Return top 20 results

  ;; Enable org-db-v3 keybindings
  (map! :map org-mode-map
        :leader
        (:prefix "s"  ; search prefix
         :desc "Semantic search org-db" "d" #'org-db-v3-semantic-search)))

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
