;;; init.el --- Vanilla Emacs config for Org + Org-Roam -*- lexical-binding: t; -*-

;; A minimal, learnable Emacs configuration focused on org-mode workflows.
;; Designed to run alongside Doom Emacs via --init-directory (Emacs 29+).
;;
;; Structure: single file with section headers for learning load order.
;; Keybindings: evil-mode + general.el for SPC leader (Doom conventions).
;; Packages: built-in package.el + use-package (zero external deps).


;;; ============================================================
;;; Section 1: Package Management
;;; ============================================================

;; Configure package archives — MELPA for community packages,
;; GNU ELPA and NonGNU ELPA for FSF-approved and non-FSF packages.
(require 'package)
(setq package-archives
      '(("melpa"    . "https://melpa.org/packages/")
        ("gnu"      . "https://elpa.gnu.org/packages/")
        ("nongnu"   . "https://elpa.nongnu.org/nongnu/")))

(package-initialize)

;; Refresh package list on first launch (when archive cache is empty).
(unless package-archive-contents
  (package-refresh-contents))

;; use-package is built-in since Emacs 29. Ensure all packages install
;; automatically — no need to manually M-x package-install.
(require 'use-package)
(setq use-package-always-ensure t)

;; Keep init.el clean — redirect M-x customize output to a separate file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))


;; git-auto-commit-mode — needed because ~/org/.dir-locals.el enables it.
;; Without this, org-roam fails to process files that trigger dir-locals eval.
(use-package git-auto-commit-mode :defer t)


;;; ============================================================
;;; Section 2: Better Defaults + Performance
;;; ============================================================

;; --- Encoding ---
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; --- Splash screen ---
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil)

(defvar thb/splash-hints
  '(("SPC ."     . "find file")
    ("SPC f r"   . "recent files")
    ("SPC n r f" . "org-roam find")
    ("SPC o a"   . "agenda")
    ("SPC q q"   . "quit"))
  "Key hints shown on the splash screen.")

(defun thb/splash-render (&optional _)
  "Render the splash screen content, centered to current window."
  (when-let* ((buf (get-buffer "*thbemacs*"))
              (win (get-buffer-window buf)))
    (with-current-buffer buf
      (let* ((inhibit-read-only t)
             (width (window-width win))
             (height (window-height win))
             (key-width (apply #'max (mapcar (lambda (h) (length (car h))) thb/splash-hints)))
             (block-width (+ key-width 3 (apply #'max (mapcar (lambda (h) (length (cdr h))) thb/splash-hints))))
             (block-pad (max 0 (/ (- width block-width) 2)))
             (title-visual-width 16)
             (title-pad (max 0 (/ (- width title-visual-width) 2)))
             (vpad (max 1 (/ (- height 7) 3))))
        (erase-buffer)
        (dotimes (_ vpad) (insert "\n"))
        (insert (make-string title-pad ?\s))
        (insert (propertize "thbemacs" 'face '(:height 2.0 :weight bold)))
        (insert "\n\n")
        (dolist (hint thb/splash-hints)
          (let* ((key (car hint))
                 (desc (cdr hint))
                 (row (concat (make-string (- key-width (length key)) ?\s)
                              key "   " desc)))
            (insert (make-string block-pad ?\s))
            (insert (propertize row 'face 'font-lock-comment-face))
            (insert "\n")))
        (goto-char (point-min))))))

(defun thb/splash-screen ()
  "Create the thbemacs splash buffer."
  (let ((buf (get-buffer-create "*thbemacs*")))
    (with-current-buffer buf
      (special-mode)
      (setq cursor-type nil)
      (add-hook 'window-size-change-functions #'thb/splash-render nil t))
    ;; Initial render after frame is ready
    (add-hook 'window-setup-hook
              (lambda () (thb/splash-render)))
    buf))

(setq initial-buffer-choice #'thb/splash-screen)

;; --- Quality of life ---
(setq use-short-answers t)                ; y/n instead of yes/no
(setq confirm-kill-emacs 'y-or-n-p)       ; confirm before quitting
(setq ring-bell-function 'ignore)         ; no beeping

;; --- Files ---
(setq create-lockfiles nil                ; no .#lockfiles
      make-backup-files nil               ; no ~ backup files
      auto-save-default t)                ; keep auto-save (recovery)

;; Backup files go to temp dir if auto-save creates them
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; --- Revert / History ---
(global-auto-revert-mode 1)              ; reload files changed on disk
(setq global-auto-revert-non-file-buffers t)

(save-place-mode 1)                       ; remember cursor position
(recentf-mode 1)                          ; track recent files
(setq recentf-max-items 200)
(savehist-mode 1)                         ; persist minibuffer history

;; --- Display ---
(global-display-line-numbers-mode 1)      ; line numbers everywhere
(column-number-mode 1)                    ; column number in modeline
(show-paren-mode 1)                       ; highlight matching parens
(setq show-paren-delay 0)

;; Disable line numbers for prose and special buffers
(dolist (mode '(org-mode-hook
               special-mode-hook
               term-mode-hook
               eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode -1))))

;; Smooth scrolling — ultra-scroll replaces pixel-scroll-precision-mode
;; with much smoother trackpad/mouse wheel behavior on macOS.
(use-package ultra-scroll
  :init
  (setq scroll-conservatively 101
        scroll-margin 0
        scroll-preserve-screen-position t)
  :config
  (ultra-scroll-mode 1))

;; --- Shell / Subprocess ---
(setq shell-file-name "/bin/bash")        ; predictable shell for subprocesses
(setenv "TERMINFO" (expand-file-name "~/.terminfo"))  ; Ghostty compat

;; --- Performance (from Doom internals + performance-config.el) ---
(setq read-process-output-max (* 4 1024 1024)  ; 4MB subprocess I/O buffer
      process-adaptive-read-buffering nil        ; disable adaptive buffering
      frame-inhibit-implied-resize t             ; don't resize frame implicitly
      inhibit-compacting-font-caches t           ; keep font caches in memory
      bidi-display-reordering 'left-to-right     ; skip bidi processing
      bidi-paragraph-direction 'left-to-right
      fast-but-imprecise-scrolling t             ; skip fontification during fast scroll
      redisplay-skip-fontification-on-input t    ; don't fontify while typing/scrolling
      jit-lock-defer-time 0                      ; defer font-lock to idle time
      echo-keystrokes 0.1                        ; show keystrokes faster
      eldoc-idle-delay 0.1
      split-width-threshold 160                  ; prefer horizontal splits
      window-combination-resize t)               ; resize windows proportionally

;; Handle long-line files gracefully
(global-so-long-mode 1)

;; Large file handler — switch to fundamental-mode for files >1MB
(defun thb/check-large-file ()
  "Disable expensive modes for files larger than 1MB."
  (when (and (buffer-file-name)
             (> (buffer-size) (* 1024 1024)))
    (fundamental-mode)
    (font-lock-mode -1)
    (message "Large file detected — disabled expensive modes")))
(add-hook 'find-file-hook #'thb/check-large-file)


;;; ============================================================
;;; Section 3: macOS Integration
;;; ============================================================

(when (eq system-type 'darwin)
  ;; Import PATH from shell so GUI Emacs finds brew/go/node/etc.
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize))

  ;; Modifier keys: Option=Meta, Command=Super, Right-Option=accents
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        mac-right-option-modifier 'none)

  ;; Use macOS trash instead of rm
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash"))


;;; ============================================================
;;; Section 4: Appearance
;;; ============================================================

;; Font — PragmataPro Mono Liga 16pt
(set-face-attribute 'default nil
                    :family "PragmataPro Mono Liga"
                    :height 160
                    :weight 'normal)

;; Font ligatures — PragmataPro Liga support via ligature.el
;; Doom's +pragmata-pro flag does this under the hood.
(use-package ligature
  :config
  (ligature-set-ligatures
   'prog-mode
   '("!!" "!!." "!=" "!==" "#!" "##" "###" "####" "#(" "#:" "#=" "#?" "#["
     "#_" "#_(" "#{" "$>" "%%" "&&" "(*" "*)" "*+" "*-" "*/" "*>" "++"
     "+++" "+:" "+>" "--" "---" "--->" "-->" "-:" "-<" "-<<" "->" "->>"
     "-|" "-~" ".-" ".." "..." "..<" ".=" ".?" "/*" "//" "///" "/="
     "/==" "/>" ":+" ":-" "://" "::" ":::" "::=" ":<" ":=" ":>" ";;"
     "<!-" "<!" "<!--" "<#" "<#--" "<$" "<$>" "<*" "<*>" "<+" "<+>"
     "<-" "<--" "<---" "<->" "<-->" "<-<" "</" "</>" "<:" "<<" "<<-"
     "<<<" "<<=" "<=" "<=<" "<==" "<=>" "<>" "<|" "<|>" "<||" "<~"
     "<~~" "=!=" "=/=" "=:" "=:=" "=<<" "==" "===" "==>" "=>" "=>>"
     ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "?." "?:" "?=" "??" "[|"
     "\\\\" "]#" "^=" "__" "_|_" "www" "{|" "|-" "|=" "|>" "|]" "||"
     "||=" "||>" "|}" "~-" "~=" "~>" "~~" "~~>"))
  ;; Only enable in prog-mode (global adds rendering cost during scroll)
  (dolist (mode '(prog-mode-hook))
    (add-hook mode #'ligature-mode)))

;; Line spacing for org-modern box rendering (0.1 = minimum recommended)
(setq-default line-spacing 0.1)

;; Modeline — nano-modeline for a clean, minimal status bar.
(use-package nano-modeline
  :init
  (setq nano-modeline-position 'nano-modeline-footer)
  :config
  (nano-modeline-text-mode t)
  (add-hook 'prog-mode-hook #'nano-modeline-prog-mode)
  (add-hook 'org-mode-hook #'nano-modeline-org-mode)
  (add-hook 'org-agenda-finalize-hook #'nano-modeline-org-agenda-mode)
  (add-hook 'magit-mode-hook #'nano-modeline-magit-mode))

;; Theme — modus-operandi (light) / modus-vivendi (dark)
;; These are built-in since Emacs 28, high-contrast and WCAG-compliant.
(load-theme 'modus-operandi t)

(defun thb/toggle-theme ()
  "Toggle between modus-operandi (light) and modus-vivendi (dark)."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi)
      (progn (disable-theme 'modus-operandi)
             (load-theme 'modus-vivendi t))
    (disable-theme 'modus-vivendi)
    (load-theme 'modus-operandi t)))

;; nano-theme — installed but not loaded by default.
;; Uncomment to use: (load-theme 'nano t)
(use-package nano-theme :defer t)


;;; ============================================================
;;; Section 5: Evil Mode + Leader Keys
;;; ============================================================

;; Evil — Vim emulation. This is the same approach Doom uses under the hood:
;; evil-mode for modal editing, general.el for leader keys, which-key for hints.

(use-package evil
  :init
  (setq evil-want-integration t        ; required before evil loads
        evil-want-keybinding nil       ; let evil-collection handle non-editing buffers
        evil-want-C-u-scroll t         ; C-u scrolls (Vim behavior)
        evil-want-Y-yank-to-eol t      ; Y yanks to EOL (modern Vim)
        evil-undo-system 'undo-redo)   ; native undo-redo (Emacs 28+)
  :config
  (evil-mode 1))

;; Evil-collection — vim bindings for magit, dired, org-agenda, etc.
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; General.el — the same library Doom uses for SPC leader bindings.
;; Defines a leader key (SPC in normal/visual, C-SPC in insert/emacs).
(use-package general
  :after evil
  :config
  (general-create-definer thb/leader
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  ;; --- SPC f: Files ---
  (thb/leader
    "f"  '(:ignore t :which-key "files")
    "ff" '(find-file :which-key "find file")
    "fr" '(consult-recent-file :which-key "recent files")
    "fs" '(save-buffer :which-key "save")
    "fS" '(write-file :which-key "save as")
    "fy" '((lambda () (interactive)
             (when-let* ((path (buffer-file-name)))
               (kill-new path)
               (message "Copied: %s" path)))
           :which-key "yank file path"))

  ;; --- SPC b: Buffers ---
  (thb/leader
    "b"  '(:ignore t :which-key "buffers")
    "bb" '(consult-buffer :which-key "switch buffer")
    "bd" '(kill-current-buffer :which-key "kill buffer")
    "bi" '(ibuffer :which-key "ibuffer")
    "bk" '(kill-current-buffer :which-key "kill buffer")
    "bn" '(next-buffer :which-key "next buffer")
    "bp" '(previous-buffer :which-key "prev buffer"))

  ;; --- SPC s: Search ---
  (thb/leader
    "s"  '(:ignore t :which-key "search")
    "ss" '(consult-line :which-key "search lines")
    "sS" '(consult-line-multi :which-key "search all buffers")
    "sp" '(consult-ripgrep :which-key "search project")
    "si" '(consult-imenu :which-key "imenu"))

  ;; --- SPC n: Notes / Org-Roam ---
  (thb/leader
    "n"  '(:ignore t :which-key "notes")
    "nr" '(:ignore t :which-key "roam")
    "nrf" '(org-roam-node-find :which-key "find node")
    "nri" '(org-roam-node-insert :which-key "insert link")
    "nrc" '(org-roam-capture :which-key "capture")
    "nrl" '(org-roam-buffer-toggle :which-key "backlinks")
    "nrs" '(consult-org-roam-search :which-key "search roam"))

  ;; --- SPC o: Open ---
  (thb/leader
    "o"  '(:ignore t :which-key "open")
    "oa" '(org-agenda :which-key "agenda")
    "oc" '(org-capture :which-key "capture"))

  ;; --- SPC g: Git ---
  (thb/leader
    "g"  '(:ignore t :which-key "git")
    "gg" '(magit-status :which-key "magit status")
    "gb" '(magit-blame :which-key "blame")
    "gl" '(magit-log-current :which-key "log"))

  ;; --- SPC t: Toggles ---
  (thb/leader
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(thb/toggle-theme :which-key "theme")
    "tl" '(display-line-numbers-mode :which-key "line numbers")
    "tw" '(whitespace-mode :which-key "whitespace")
    "tz" '(delete-other-windows :which-key "zen (one window)"))

  ;; --- SPC h: Help ---
  (thb/leader
    "h"  '(:ignore t :which-key "help")
    "hf" '(describe-function :which-key "describe function")
    "hv" '(describe-variable :which-key "describe variable")
    "hk" '(describe-key :which-key "describe key")
    "hm" '(describe-mode :which-key "describe mode")
    "hi" '(info :which-key "info manual")
    "ht" '(consult-theme :which-key "choose theme"))

  ;; --- SPC w: Windows ---
  (thb/leader
    "w"  '(:ignore t :which-key "windows")
    "wv" '(split-window-right :which-key "split vertical")
    "ws" '(split-window-below :which-key "split horizontal")
    "wd" '(delete-window :which-key "delete window")
    "ww" '(other-window :which-key "other window")
    "wm" '(delete-other-windows :which-key "maximize"))

  ;; --- SPC q: Quit ---
  (thb/leader
    "q"  '(:ignore t :which-key "quit")
    "qq" '(save-buffers-kill-emacs :which-key "quit")
    "qQ" '(kill-emacs :which-key "quit without saving")
    "qr" '((lambda () (interactive) (load-file (expand-file-name "init.el" user-emacs-directory)) (message "init.el reloaded")) :which-key "reload init.el")
    "qR" '(thb/restart-emacs-restore :which-key "restart"))

  ;; --- SPC p: Project ---
  (thb/leader
    "p"  '(:ignore t :which-key "project")
    "pp" '(project-switch-project :which-key "switch project")
    "pf" '(project-find-file :which-key "find file")
    "ps" '(consult-ripgrep :which-key "search project")
    "pb" '(project-switch-to-buffer :which-key "project buffer")
    "pk" '(project-kill-buffers :which-key "kill buffers"))

  ;; --- SPC c: Code ---
  (thb/leader
    "c"  '(:ignore t :which-key "code")
    "cx" '(eval-last-sexp :which-key "eval sexp")
    "cb" '(eval-buffer :which-key "eval buffer")
    "cr" '(eval-region :which-key "eval region"))

  ;; --- SPC SPC: M-x ---
  (thb/leader
    "SPC" '(execute-extended-command :which-key "M-x")
    ":"   '(execute-extended-command :which-key "M-x")
    ";"   '(eval-expression :which-key "eval expression")
    "/"   '(consult-ripgrep :which-key "search project")
    "."   '(find-file :which-key "find file")
    ","   '(consult-buffer :which-key "switch buffer")
    "<"   '(consult-buffer-other-window :which-key "buffer other window")
    "`"   '(evil-switch-to-windows-last-buffer :which-key "last buffer")
    "u"   '(universal-argument :which-key "universal arg")
    "x"   '(scratch-buffer :which-key "scratch buffer")))

;; Which-key — show available keybindings after pressing a prefix.
(use-package which-key
  :config
  (setq which-key-idle-delay 0.4        ; faster than default (1.0)
        which-key-sort-order 'which-key-key-order-alpha)
  (which-key-mode 1))


;;; ============================================================
;;; Section 6: Completion (Vertico Stack)
;;; ============================================================

;; Vertico — vertical minibuffer completion UI (replaces Helm/Ivy).
(use-package vertico
  :init (vertico-mode 1)
  :config
  (setq vertico-cycle t
        vertico-count 15))

;; Orderless — flexible matching (space-separated components, regex, flex).
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion))
                                        (org-roam-node (display-sort-function . identity)))))

;; Marginalia — rich annotations in the minibuffer (file sizes, docstrings).
(use-package marginalia
  :init (marginalia-mode 1))

;; Consult — enhanced search/navigation commands (replaces counsel/swiper).
(use-package consult
  :config
  (setq consult-narrow-key "<"))         ; narrow results with <

;; Embark — contextual actions on minibuffer candidates (like right-click).
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

;; Embark-consult — integration between embark and consult.
(use-package embark-consult
  :after (embark consult))


;;; ============================================================
;;; Section 7: Org-Mode
;;; ============================================================

(use-package org
  :ensure nil                            ; built-in, don't install from MELPA
  :config
  ;; --- Directories ---
  (setq org-directory "~/org/"
        org-agenda-files (list (expand-file-name "todo.org" org-directory)))

  ;; --- Indentation ---
  ;; Virtual indentation based on heading level (no extra spaces in file).
  (setq org-startup-indented t)

  ;; Word wrap — wrap at word boundaries, respecting org-indent.
  (add-hook 'org-mode-hook #'visual-line-mode)

  ;; --- TODO Keywords ---
  ;; Matches Doom config exactly — same states work in both editors.
  (setq org-todo-keywords
        '((sequence "ICEBOX(I)" "BACKLOG(b)" "TO-DO(T)" "TODO(t)" "IN-PROGRESS(i)" "PAUSED(p)" "BLOCKED(B)" "|" "DONE(d)" "CANCELLED(c)")))

  ;; --- Priorities ---
  (setq org-priority-highest ?A
        org-priority-lowest ?C
        org-priority-default ?B)

  ;; --- Logging ---
  ;; Record timestamps for state changes, store in LOGBOOK drawer.
  (setq org-log-done 'time
        org-log-into-drawer t
        org-treat-insert-todo-heading-as-state-change t
        org-log-repeat 'time
        org-log-reschedule 'time
        org-log-redeadline 'time)

  ;; --- Auto-save ---
  ;; Save org buffers periodically (protects against data loss).
  (add-hook 'org-mode-hook #'auto-save-mode)
  (setq auto-save-visited-interval 10)
  (auto-save-visited-mode 1)

  ;; --- Capture Templates ---
  ;; Start with two basic templates. Full set commented out below.
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "todo.org" "Inbox")
           "** TODO %?\nSCHEDULED: %t\n")
          ("r" "Research" entry (file+headline "todo.org" "Inbox")
           "** TODO %? :research:\n")))

  ;; --- Agenda ---
  (setq org-agenda-start-day nil
        org-agenda-start-on-weekday nil
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t)

  ;; Auto-save after agenda edits
  (advice-add 'org-agenda-todo :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-agenda-priority :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-agenda-schedule :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-agenda-deadline :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-agenda-set-tags :after (lambda (&rest _) (org-save-all-org-buffers)))
  (advice-add 'org-agenda-refile :after (lambda (&rest _) (org-save-all-org-buffers)))

  ;; --- Link navigation ---
  (defun thb/org-open-link-other-window ()
    "Open org link at point in another window."
    (interactive)
    (let ((org-link-frame-setup
           (cl-acons 'file #'find-file-other-window org-link-frame-setup)))
      (org-open-at-point)))

  (thb/leader
    :keymaps 'org-mode-map
    "m"  '(:ignore t :which-key "org")
    "ml" '(:ignore t :which-key "links")
    "mlo" '(org-open-at-point :which-key "open link")
    "mls" '(thb/org-open-link-other-window :which-key "open in split")))

;; ----- Commented-out: Full Capture Templates -----
;; Uncomment these and replace the basic templates above as you learn.
;;
;; (setq org-capture-templates
;;       '(("t" "Todo" entry (file+headline "todo.org" "Inbox")
;;          "** TODO %?\nSCHEDULED: %t\n")
;;         ("i" "Interrupt" entry (file+olp "todo.org" "Areas" "Interrupt")
;;          "*** TODO %? :interrupt:\nSCHEDULED: %t\n")
;;         ("e" "Enablement" entry (file+olp "todo.org" "Areas" "Enablement")
;;          "*** TODO %? :enablement:\nSCHEDULED: %t\n")
;;         ("c" "Compliance" entry (file+olp "todo.org" "Areas" "Compliance")
;;          "*** TODO %? :compliance:\nSCHEDULED: %t\n")
;;         ("l" "Leadership" entry (file+olp "todo.org" "Areas" "Leadership")
;;          "*** TODO %? :leadership:\nSCHEDULED: %t\n")
;;         ("p" "Personal" entry (file+olp "todo.org" "Areas" "Personal")
;;          "*** TODO %? :personal:\nSCHEDULED: %t\n")
;;         ("r" "Research" entry (file+headline "todo.org" "Inbox")
;;          "** TODO %? :research:\n")))

;; ----- Commented-out: Org-Super-Agenda -----
;; Install and enable when ready for advanced agenda views.
;;
;; (use-package org-super-agenda
;;   :after org-agenda
;;   :config
;;   (org-super-agenda-mode 1)
;;
;;   (setq thb/org-super-agenda-groups-focus
;;         '((:discard (:todo ("PROJ" "BACKLOG" "CANCELLED")))
;;           (:name "Urgent" :tag "oncall" :priority "A")
;;           (:name "Blocked" :todo "BLOCKED")
;;           (:name "In progress" :todo "IN-PROGRESS" :order 1)
;;           (:name "Overdue" :and (:scheduled past :todo ("TODO" "PAUSED")) :order 2)
;;           (:name "Today" :and (:scheduled today :todo "TODO") :order 3)
;;           (:name "Deadlines" :deadline past :deadline today :deadline future :order 4)
;;           (:discard (:scheduled future))
;;           (:discard (:todo "PAUSED"))
;;           (:name "Other" :todo "TODO" :order 5)))
;;
;;   (setq thb/org-super-agenda-groups-full
;;         '((:name "Urgent" :tag "oncall" :priority "A")
;;           (:name "Blocked" :todo "BLOCKED")
;;           (:name "In progress (today)" :and (:todo "IN-PROGRESS" :scheduled today))
;;           (:name "In progress (overdue)" :and (:todo "IN-PROGRESS" :scheduled past))
;;           (:name "Today" :and (:todo "TODO" :scheduled today))
;;           (:name "Deadlines" :deadline future :deadline today)
;;           (:name "Paused" :todo "PAUSED")
;;           (:name "Overdue" :and (:scheduled past :not (:todo "IN-PROGRESS")))
;;           (:name "Future" :scheduled future)
;;           (:name "Other" :anything t)))
;;
;;   (setq org-super-agenda-groups thb/org-super-agenda-groups-focus))

;; ----- Commented-out: Custom Agenda Commands -----
;;
;; (setq org-agenda-custom-commands
;;       '(("f" "Today's Focus"
;;          ((agenda ""
;;                   ((org-agenda-span 1)
;;                    (org-agenda-start-day nil)
;;                    (org-super-agenda-groups thb/org-super-agenda-groups-focus)
;;                    (org-agenda-overriding-header "Today\n"))))
;;          ((org-agenda-compact-blocks t)))
;;         ("a" "Full Agenda"
;;          ((agenda ""
;;                   ((org-agenda-span 7)
;;                    (org-agenda-start-day nil)
;;                    (org-super-agenda-groups thb/org-super-agenda-groups-full)
;;                    (org-agenda-overriding-header "Agenda\n"))))
;;          ((org-agenda-compact-blocks t)))
;;         ("b" "Blocked Items"
;;          ((todo "BLOCKED"
;;                 ((org-agenda-overriding-header "Blocked\n"))))
;;          ((org-agenda-compact-blocks t)))
;;         ("i" "In Progress"
;;          ((todo "IN-PROGRESS"
;;                 ((org-agenda-overriding-header "In Progress\n")
;;                  (org-agenda-sorting-strategy '(priority-down scheduled-up)))))
;;          ((org-agenda-compact-blocks t)))
;;         ("w" "Weekly Review"
;;          ((agenda ""
;;                   ((org-agenda-span 'week)
;;                    (org-agenda-start-on-weekday 1)
;;                    (org-super-agenda-groups thb/org-super-agenda-groups-full)
;;                    (org-agenda-overriding-header "Weekly\n"))))
;;          ((org-agenda-compact-blocks t)))))

;; ----- Commented-out: Agenda View Toggle -----
;;
;; (defun thb/org-agenda-toggle-view ()
;;   "Toggle between focus and full agenda views."
;;   (interactive)
;;   (if (equal org-super-agenda-groups thb/org-super-agenda-groups-focus)
;;       (progn
;;         (setq org-super-agenda-groups thb/org-super-agenda-groups-full)
;;         (message "Full view"))
;;     (setq org-super-agenda-groups thb/org-super-agenda-groups-focus)
;;     (message "Focus view"))
;;   (when (bound-and-true-p org-agenda-buffer-name)
;;     (org-agenda-redo-all)))


;;; ============================================================
;;; Section 8: Org-Roam
;;; ============================================================

(use-package org-roam
  :after org
  :config
  (setq org-roam-directory (expand-file-name "roam/" org-directory)
        org-roam-node-default-sort 'file-mtime
        org-roam-node-display-template
        (concat (propertize "${modified:22}" 'face 'font-lock-comment-face)
                " ${title:*} "
                (propertize "${orgtags}" 'face 'org-tag)))

  (cl-defmethod org-roam-node-modified ((node org-roam-node))
    "Return file modification time as org inactive timestamp."
    (let ((mtime (org-roam-node-file-mtime node)))
      (if mtime
          (format-time-string "[%Y-%m-%d %a %H:%M]" mtime)
        "")))

  (cl-defmethod org-roam-node-orgtags ((node org-roam-node))
    "Return tags formatted as org filetags (:tag1:tag2:)."
    (let ((tags (org-roam-node-tags node)))
      (if tags
          (concat ":" (string-join tags ":") ":")
        "")))

  ;; Capture templates — timestamped default + named slug
  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :target (file+head "%<%Y%m%d%H%M>.org"
                              "#+title: ${title}\n#+date: %<%Y-%m-%d>\n#+created: %U\n\n")
           :unnarrowed t)
          ("n" "named note" plain "%?"
           :target (file+head "${slug}.org"
                              "#+title: ${title}\n#+date: %<%Y-%m-%d>\n\n")
           :unnarrowed t)))

  (org-roam-db-autosync-mode 1))

;; Consult-org-roam — search roam notes with consult/ripgrep.
(use-package consult-org-roam
  :after org-roam
  :config
  (setq consult-org-roam-grep-func #'consult-ripgrep))


;;; ============================================================
;;; Section 9: Org-Modern
;;; ============================================================

;; Visual improvements for org-mode: pretty headings, lists, tables, TODOs.
(use-package org-modern
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-star 'replace
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((42 . "◦") (43 . "•") (45 . "–"))
        org-modern-keyword t
        org-modern-block-fringe t
        org-modern-todo t))

;; Label face — slightly smaller for cleaner TODO boxes
(custom-set-faces
 '(org-modern-label ((t (:height 0.85 :width normal :weight regular)))))

;; Heading sizes — visual hierarchy like Doom
(custom-set-faces
 '(org-level-1 ((t (:height 1.3 :weight bold))))
 '(org-level-2 ((t (:height 1.15 :weight bold))))
 '(org-level-3 ((t (:height 1.05 :weight semi-bold))))
 '(org-document-title ((t (:height 1.4 :weight bold)))))

;; Source block background — subtle tint to distinguish code from prose
(custom-set-faces
 '(org-block ((t (:background "#f2ede9" :extend t))))
 '(org-block-begin-line ((t (:background "#e8e3df" :extend t :foreground "#7c6f64"))))
 '(org-block-end-line ((t (:background "#e8e3df" :extend t :foreground "#7c6f64")))))

;; Org-appear — hide markup until cursor enters (~code~, *bold*, etc.)
;; Manual trigger: only expand links/emphasis in evil insert mode.
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-trigger 'manual)
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'evil-insert-state-entry-hook #'org-appear-manual-start nil t)
              (add-hook 'evil-insert-state-exit-hook #'org-appear-manual-stop nil t))))

;; Required for org-appear to work
(setq org-hide-emphasis-markers t)

;; Org-tidy — hide property drawers, show inline indicator instead.
(use-package org-tidy
  :hook (org-mode . org-tidy-mode)
  :config
  (setq org-tidy-properties-style 'inline
        org-tidy-top-property-style 'invisible
        org-tidy-protect-fontification nil))


;;; ============================================================
;;; Section 10: Version Control + Local Overrides
;;; ============================================================

;; Magit — the definitive Git interface for Emacs.
(use-package magit
  :commands (magit-status magit-blame magit-log-current)
  :config
  ;; Performance tuning (from performance-config.el)
  (setq magit-revision-show-gravatars nil
        magit-diff-refine-hunk nil
        magit-section-cache-visibility t
        magit-log-section-commit-count 10
        magit-section-initial-visibility-alist
        '((stashes . hide)
          (untracked . hide)
          (unpushed . hide)
          (unpulled . hide))
        magit-refresh-verbose nil
        magit-diff-highlight-trailing nil
        ;; Reduced status sections for faster display
        magit-status-sections-hook
        '(magit-insert-status-headers
          magit-insert-merge-log
          magit-insert-rebase-sequence
          magit-insert-am-sequence
          magit-insert-sequencer-sequence
          magit-insert-bisect-output
          magit-insert-bisect-rest
          magit-insert-bisect-log
          magit-insert-untracked-files
          magit-insert-unstaged-changes
          magit-insert-staged-changes)))

;; Diff-hl — show VCS changes in the gutter (fringe).
(use-package diff-hl
  :hook ((prog-mode . diff-hl-mode)
         (org-mode  . diff-hl-mode)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (setq diff-hl-draw-borders nil))

;; --- Restart with Session Restore ---
(defun thb/restart-emacs-restore ()
  "Save current session and restart Emacs, restoring open buffers."
  (interactive)
  (let ((dir (expand-file-name "restart/" user-emacs-directory)))
    (make-directory dir t)
    (desktop-save dir t)
    (restart-emacs)))

;; Restore session after restart (runs once, then cleans up)
(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((dir (expand-file-name "restart/" user-emacs-directory))
                  (file "restart/.emacs.desktop"))
              (when (file-exists-p (expand-file-name file user-emacs-directory))
                (desktop-read dir)
                (delete-file (expand-file-name ".emacs.desktop" dir))
                (let ((lock (expand-file-name ".emacs.desktop.lock" dir)))
                  (when (file-exists-p lock) (delete-file lock)))))))

;; --- Local Overrides ---
;; Load machine-specific settings from local.el (not tracked in git).
;; Copy local.el.example to local.el and customize.
(let ((local-config (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-config)
    (load local-config 'noerror 'nomessage)))


;;; init.el ends here
