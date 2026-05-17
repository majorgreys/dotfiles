;;; init.el --- Vanilla Emacs config for Org + Org-Roam -*- lexical-binding: t; -*-

;; A minimal, learnable Emacs configuration focused on org-mode workflows.
;; Designed to run alongside Doom Emacs via --init-directory (Emacs 29+).
;;
;; Structure: single file with section headers for learning load order.
;; Keybindings: evil-mode + general.el for SPC leader (Doom conventions).
;; Packages: built-in package.el + use-package (zero external deps).


;;; ============================================================
;;; Package Management
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



;;; ============================================================
;;; Better Defaults + Performance
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
    ("SPC n f"   . "vulpea find")
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

;; Hide .beads/ contents from project-find-file.  ~/work/ tracks
;; .beads/issues.jsonl per project on purpose; we just don't want those
;; files swamping completion candidates.
(define-advice project-files (:filter-return (files) thb/skip-beads)
  (seq-remove (lambda (f) (string-match-p "/\\.beads/" f)) files))

;; --- Display ---
(global-display-line-numbers-mode 1)      ; line numbers everywhere
(column-number-mode 1)                    ; column number in modeline
(show-paren-mode 1)                       ; highlight matching parens
(setq show-paren-delay 0)

;; Disable line numbers for prose and special buffers
(defun thb/disable-line-numbers ()
  "Disable line numbers in current buffer."
  (display-line-numbers-mode -1))
(dolist (mode '(org-mode-hook
               org-agenda-mode-hook
               markdown-ts-mode-hook
               special-mode-hook
               term-mode-hook
               eshell-mode-hook))
  (add-hook mode #'thb/disable-line-numbers))

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
;;; macOS Integration
;;; ============================================================

(when (eq system-type 'darwin)
  ;; Import PATH from shell so GUI Emacs finds brew/go/node/etc.
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize))

  ;; LaunchAgent daemons start with a stripped PATH; ensure user bin dirs
  ;; (br, bd, cargo, go) are reachable for subprocesses regardless.
  (dolist (dir '("~/.local/bin" "~/go/bin" "~/.cargo/bin"))
    (let ((d (expand-file-name dir)))
      (when (file-directory-p d)
        (add-to-list 'exec-path d)
        (setenv "PATH" (concat d ":" (getenv "PATH"))))))

  ;; Modifier keys: Option=Meta, Command=Super, Right-Option=accents
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super
        mac-right-option-modifier 'none)

  ;; Use macOS trash instead of rm
  (setq delete-by-moving-to-trash t
        trash-directory "~/.Trash"))


;;; ============================================================
;;; Appearance
;;; ============================================================

;; Font — PragmataPro Mono Liga 16pt
(set-face-attribute 'default nil
                    :family "PragmataPro Mono Liga"
                    :height 160
                    :weight 'normal)
;; Match fixed-pitch to default so org tables / code / inline-code don't fall
;; back to the system "Monospace" (Menlo/Courier) which doesn't match the
;; surrounding text. modus-themes-fixed-pitch inherits from this.
(set-face-attribute 'fixed-pitch nil
                    :family "PragmataPro Mono Liga"
                    :height 1.0)

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
  (add-hook 'prog-mode-hook #'ligature-mode))

;; Line spacing for org-modern box rendering (0.1 = minimum recommended)
(setq-default line-spacing 0.1)

;; Modeline — mood-line for a clean, lightweight status bar.
(use-package mood-line
  :config
  (setq mood-line-glyph-alist
        (append '((:vc-added . ?+))          ; U+1F7A4 not in PragmataPro — use +
                mood-line-glyphs-unicode))

  ;; Workspace indicator in modeline — shows [workspace] before buffer name
  (defun thb/modeline-workspace ()
    "Return current workspace name for modeline."
    (when-let ((tab (ignore-errors (tab-bar--current-tab))))
      (propertize (format "[%s]" (alist-get 'name tab))
                  'face 'font-lock-constant-face)))

  ;; Set format BEFORE enabling mode
  (setq mood-line-format
        (mood-line-defformat
         :left  (((mood-line-segment-buffer-status) . " ")
                 ((thb/modeline-workspace) . " ")
                 ((mood-line-segment-buffer-name) . " ")
                 ((mood-line-segment-vc) . " "))
         :right (((mood-line-segment-cursor-position) . " ")
                 ((mood-line-segment-major-mode) . " "))))

  (mood-line-mode 1))

;; Mixed-pitch — variable-pitch fonts for prose, fixed-pitch for code/tables.
;; Off by default (PragmataPro Mono is the global default); toggle with SPC t p.
(use-package mixed-pitch
  :commands mixed-pitch-mode
  :config
  (setq mixed-pitch-set-height nil))   ; preserve existing line-height

;; Olivetti — narrow, centered text body (lighter than writeroom-mode).
;; Per-buffer toggle via SPC t o; suitable for daily reading of org notes.
(use-package olivetti
  :commands olivetti-mode
  :config
  (setq olivetti-body-width 120
        olivetti-style 'fancy
        olivetti-minimum-body-width 80)
  ;; Drop line numbers when olivetti is on; restore on toggle off.
  (defvar-local thb/olivetti-prev-line-numbers nil)
  (add-hook 'olivetti-mode-on-hook
            (lambda ()
              (setq thb/olivetti-prev-line-numbers
                    (bound-and-true-p display-line-numbers-mode))
              (display-line-numbers-mode -1)))
  (add-hook 'olivetti-mode-off-hook
            (lambda ()
              (when thb/olivetti-prev-line-numbers
                (display-line-numbers-mode 1)))))

;; Zen mode — writeroom-mode for distraction-free writing (like Doom's +zen).
;; Centers text column, scales text up, hides visual noise. Per-buffer only.
(use-package writeroom-mode
  :commands writeroom-mode
  :config
  (setq writeroom-global-effects nil          ; no global side effects
        writeroom-maximize-window nil          ; don't delete other windows
        writeroom-width 80                     ; column width
        writeroom-mode-line t                  ; keep modeline visible
        writeroom-bottom-divider-width 0)

  (defvar thb/zen-text-scale 2
    "Text-scaling level when zen mode is active.")

  (add-hook 'writeroom-mode-hook
            (lambda ()
              (text-scale-set (if writeroom-mode thb/zen-text-scale 0))
              (display-line-numbers-mode (if writeroom-mode -1 1))
              (visual-fill-column-adjust)))

  (advice-add #'text-scale-adjust :after #'visual-fill-column-adjust))

;; Theme — modus-operandi-tinted (light) / modus-vivendi-tinted (dark)
;; These are built-in since Emacs 28, high-contrast and WCAG-compliant.
;; Default to the dark tinted variant to match Ghostty's terminal theme.
(load-theme 'modus-vivendi-tinted t)

;; Enable 24-bit color on terminals with COLORTERM=truecolor.
;; Emacs's tty-color-24bit only activates when display-color-cells == 16777216,
;; but terminfo caps colors at 32767.  This advice bypasses that check when
;; the terminal advertises truecolor support, allowing the ghostty-direct
;; terminfo's setaf/setab to emit \e[38;2;R;G;Bm escape sequences.
(defun thb/tty-color-24bit-truecolor (orig-fn rgb &optional display)
  "Enable 24-bit color translation when COLORTERM=truecolor."
  (if (and rgb
           (not (display-graphic-p display))
           (equal (getenv "COLORTERM" display) "truecolor"))
      (let ((r (ash (car rgb) -8))
            (g (ash (cadr rgb) -8))
            (b (ash (nth 2 rgb) -8)))
        (logior (ash r 16) (ash g 8) b))
    (funcall orig-fn rgb display)))
(advice-add 'tty-color-24bit :around #'thb/tty-color-24bit-truecolor)

;; Re-apply theme for new frames (terminal frames from emacsclient -nw).
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame
              (unless (display-graphic-p)
                (tty-run-terminal-initialization frame "xterm" t))
              (when-let ((theme (car custom-enabled-themes)))
                (load-theme theme t)))))

;; Map terminal types to xterm so Emacs loads term/xterm.el for key handling.
(add-to-list 'term-file-aliases '("xterm-ghostty" . "xterm"))
(add-to-list 'term-file-aliases '("ghostty-direct" . "xterm"))
(add-to-list 'term-file-aliases '("xterm-direct" . "xterm"))

(defun thb/apply-org-faces ()
  "Set org faces: theme-aware block colors + heading/label sizes.
In GUI frames, set block backgrounds from theme palette.
In TUI frames, skip backgrounds to avoid 256-color approximation issues."
  (when (display-graphic-p)
    (let ((bg  (modus-themes-get-color-value 'bg-dim))
          (fg  (modus-themes-get-color-value 'fg-dim))
          (bgm (modus-themes-get-color-value 'bg-inactive))
          (bgq (modus-themes-get-color-value 'bg-blue-nuanced))
          (bge (modus-themes-get-color-value 'bg-green-nuanced)))
      (set-face-attribute 'org-block nil :background bg :extend t)
      (set-face-attribute 'org-block-begin-line nil :background bgm :foreground fg :extend t)
      (set-face-attribute 'org-block-end-line nil :background bgm :foreground fg :extend t)
      (when (facep 'org-quote)
        (set-face-attribute 'org-quote nil :background bgq :slant 'italic :extend t))
      (when (facep 'org-verse)
        (set-face-attribute 'org-verse nil :background bge :slant 'italic :extend t))))
  (set-face-attribute 'org-document-title nil :height 1.4 :weight 'bold)
  (set-face-attribute 'org-level-1 nil :height 1.3 :weight 'bold)
  (set-face-attribute 'org-level-2 nil :height 1.15 :weight 'bold)
  (set-face-attribute 'org-level-3 nil :height 1.05 :weight 'semi-bold)
  (when (facep 'org-modern-label)
    (set-face-attribute 'org-modern-label nil :height 0.85 :width 'normal :weight 'regular)))

(defun thb/toggle-theme ()
  "Toggle between modus-operandi-tinted (light) and modus-vivendi-tinted (dark)."
  (interactive)
  (if (eq (car custom-enabled-themes) 'modus-operandi-tinted)
      (progn (disable-theme 'modus-operandi-tinted)
             (load-theme 'modus-vivendi-tinted t))
    (disable-theme 'modus-vivendi-tinted)
    (load-theme 'modus-operandi-tinted t))
  (thb/apply-org-faces)
  (when (fboundp 'thb/apply-markdown-ts-faces)
    (thb/apply-markdown-ts-faces)))

;; Apply org faces once org-mode is available (faces don't exist until then).
(with-eval-after-load 'org
  (thb/apply-org-faces))
;; Re-apply after org-modern loads so org-modern-label face gets customized.
(with-eval-after-load 'org-modern
  (thb/apply-org-faces))

(defvar thb/big-font-base-height (face-attribute 'default :height)
  "Original default face height, recorded at init.")
(defvar thb/big-font-increment 50 "1/10-pt to add when big font mode is active.")

(defun thb/big-font-mode ()
  "Toggle big font mode (increase default face height)."
  (interactive)
  (let ((big-p (> (face-attribute 'default :height) thb/big-font-base-height)))
    (set-face-attribute 'default nil
                        :height (if big-p
                                    thb/big-font-base-height
                                  (+ thb/big-font-base-height thb/big-font-increment)))
    (message "Big font mode %s" (if big-p "off" "on"))))


;;; ============================================================
;;; Evil Mode + Leader Keys
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
  :init
  (setq evil-collection-setup-minibuffer t)  ; enable j/k in minibuffer (normal state)
  :config
  (evil-collection-init
   '(corfu consult dired embark ibuffer info magit minibuffer
     org org-roam vertico which-key diff-hl eglot
     go-mode help custom tab-bar)))

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
    "bp" '(previous-buffer :which-key "prev buffer")
    "br" '(revert-buffer-quick :which-key "revert buffer"))

  ;; --- SPC s: Search ---
  (thb/leader
    "s"  '(:ignore t :which-key "search")
    "ss" '(consult-line :which-key "search lines")
    "sS" '(consult-line-multi :which-key "search all buffers")
    "sp" '(consult-ripgrep :which-key "search project")
    "si" '(consult-imenu :which-key "imenu"))

  ;; --- SPC n: Notes (Vulpea) ---
  (thb/leader
    "n"  '(:ignore t :which-key "notes")
    "nf" '(vulpea-find :which-key "find note")
    "ni" '(vulpea-insert :which-key "insert link")
    "nb" '(vulpea-find-backlink :which-key "backlinks")
    "ns" '(consult-vulpea-grep :which-key "search notes")
    "nj" '(vulpea-journal :which-key "today's journal")
    "nd" '(vulpea-journal-date :which-key "journal by date")
    "nS" '(vulpea-ui-sidebar-toggle :which-key "toggle sidebar"))

  ;; --- SPC g: Git ---
  (thb/leader
    "g"  '(:ignore t :which-key "git")
    "gg" '(magit-status :which-key "magit status")
    "gb" '(magit-blame :which-key "blame")
    "gl" '(magit-log-current :which-key "log"))

  ;; --- SPC t: Toggles ---
  (thb/leader
    "t"  '(:ignore t :which-key "toggles")
    "tb" '(thb/big-font-mode :which-key "big font")
    "tt" '(thb/toggle-theme :which-key "theme")
    "tl" '(display-line-numbers-mode :which-key "line numbers")
    "tw" '(whitespace-mode :which-key "whitespace")
    "tz" '(writeroom-mode :which-key "zen mode")
    "ti" '(org-toggle-inline-images :which-key "inline images")
    "to" '(olivetti-mode :which-key "olivetti (centered)")
    "tp" '(mixed-pitch-mode :which-key "mixed pitch")
    "td" '(org-tidy-mode :which-key "tidy drawers")
    "tc" '(org-columns :which-key "column view (org)"))

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
    "qq" '(save-buffers-kill-terminal :which-key "close frame")
    "qQ" '(save-buffers-kill-emacs :which-key "kill daemon")
    "qr" '((lambda () (interactive)
              (package-refresh-contents)
              (load-file (expand-file-name "init.el" user-emacs-directory))
              (message "init.el reloaded"))
           :which-key "reload init.el")
    "qR" '(thb/restart-emacs-restore :which-key "restart daemon"))

  ;; --- SPC p: Project ---
  (thb/leader
    "p"  '(:ignore t :which-key "project")
    "pp" '(project-switch-project :which-key "switch project")
    "pf" '(project-find-file :which-key "find file")
    "ps" '(consult-ripgrep :which-key "search project")
    "pb" '(project-switch-to-buffer :which-key "project buffer")
    "pk" '(project-kill-buffers :which-key "kill buffers"))

  ;; --- SPC i: Issues (Beads) ---
  (thb/leader
    "i"  '(:ignore t :which-key "issues")
    "ii" '(beads :which-key "issue list")
    "ip" '(beads-project-list :which-key "project issues")
    "ic" '(beads-create-issue :which-key "create issue")
    "ia" '(beads-activity :which-key "activity feed")
    "is" '(beads-stats :which-key "stats"))

  ;; --- SPC c: Code ---
  (thb/leader
    "c"  '(:ignore t :which-key "code")
    "cx" '(eval-last-sexp :which-key "eval sexp")
    "cb" '(eval-buffer :which-key "eval buffer")
    "cr" '(eval-region :which-key "eval region"))

  ;; --- SPC SPC: project find file (matches Doom) ---
  (thb/leader
    "SPC" '(project-find-file :which-key "find file in project")
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
;;; Completion (Vertico Stack)
;;; ============================================================

;; Vertico — vertical minibuffer completion UI (replaces Helm/Ivy).
(use-package vertico
  :init (vertico-mode 1)
  :config
  (setq vertico-cycle t
        vertico-count 15)
  ;; C-j/C-k to navigate candidates in insert state (Doom convention).
  ;; In normal state (ESC), j/k work via evil-collection-vertico.
  (define-key vertico-map (kbd "C-j") #'vertico-next)
  (define-key vertico-map (kbd "C-k") #'vertico-previous))

;; Orderless — flexible matching (space-separated components, regex, flex).
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion))
)))

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

;; Corfu — in-buffer completion popup (complements Vertico for minibuffer).
(use-package corfu
  :init (global-corfu-mode 1)
  :config
  (setq corfu-auto t              ; show popup automatically
        corfu-auto-delay 0.1      ; quick popup after typing
        corfu-auto-prefix 1       ; trigger after 1 character
        corfu-cycle t             ; cycle through candidates
        corfu-preselect 'prompt)) ; don't auto-select first candidate


;;; ============================================================
;;; Workspaces (Tabspaces)
;;; ============================================================

;; Tabspaces — workspace-like buffer isolation using built-in tab-bar + project.el.
;; Configured to feel like Doom's persp-mode: tab bar shows workspace names,
;; modeline shows [workspace] indicator, quick create/delete bindings.
(use-package tabspaces
  :after (consult general)
  :init
  ;; Drop the global C-c TAB prefix; we use SPC TAB instead and C-c TAB
  ;; is org's `org-table-toggle-column-width' default — keep that for org.
  (setq tabspaces-keymap-prefix nil)
  :config
  (setq tabspaces-use-filtered-buffers-as-default t  ; buffer isolation per tab
        tabspaces-default-tab "main"                 ; name for the initial tab
        tabspaces-include-buffers '("*scratch*" "*Messages*"))

  ;; Tab bar: show workspace names (not buffer names) like Doom's +workspace/display
  (setq tab-bar-show t
        tab-bar-close-button-show nil
        tab-bar-new-button-show nil
        tab-bar-tab-name-function #'tab-bar-tab-name-all
        tab-bar-format '(tab-bar-format-tabs tab-bar-separator))

  ;; Style: highlight current tab, dim others
  (set-face-attribute 'tab-bar nil :inherit 'mode-line-inactive :box nil)
  (set-face-attribute 'tab-bar-tab nil :inherit 'mode-line :weight 'bold :box nil)
  (set-face-attribute 'tab-bar-tab-inactive nil :inherit 'mode-line-inactive :box nil)

  ;; Integrate with consult: SPC , shows only tab-local buffers
  (with-eval-after-load 'consult
    (consult-customize consult-source-buffer
                       :narrow ?b
                       :hidden t)
    (defvar consult--source-tabspaces
      `(:name "Tab Buffers"
        :narrow ?B
        :category buffer
        :face consult-buffer
        :history buffer-name-history
        :action ,#'consult--buffer-action
        :items ,(lambda () (consult--buffer-query
                            :predicate #'tabspaces--local-buffer-p
                            :sort 'visibility
                            :as #'buffer-name)))
      "Buffer source for tab-local buffers in `consult-buffer'.")
    (add-to-list 'consult-buffer-sources 'consult--source-tabspaces))

  ;; Quick workspace creation (like Doom's +workspace/new)
  (defun thb/workspace-new ()
    "Create a new workspace with an auto-generated name."
    (interactive)
    (let ((name (format "workspace-%d" (length (tab-bar-tabs)))))
      (tabspaces-switch-or-create-workspace name)
      (message "Created workspace: %s" name)))

  ;; SPC TAB: workspace bindings (matches Doom conventions)
  (thb/leader
    "TAB"   '(:ignore t :which-key "workspaces")
    "TAB n" '(tabspaces-switch-or-create-workspace :which-key "new/switch")
    "TAB N" '(thb/workspace-new :which-key "new (quick)")
    "TAB p" '(tabspaces-open-or-create-project-and-workspace :which-key "open project")
    "TAB d" '(tabspaces-kill-buffers-close-workspace :which-key "close")
    "TAB r" '(tabspaces-rename-existing-tab :which-key "rename")
    "TAB c" '(tabspaces-clear-buffers :which-key "clear buffers")
    "TAB b" '(tabspaces-switch-to-buffer :which-key "switch buffer (tab)")
    "TAB B" '(tabspaces-switch-buffer-and-tab :which-key "switch buffer+tab")
    "TAB TAB" '(tab-bar-switch-to-tab :which-key "switch tab")
    "TAB 1" '((lambda () (interactive) (tab-bar-select-tab 1)) :which-key "tab 1")
    "TAB 2" '((lambda () (interactive) (tab-bar-select-tab 2)) :which-key "tab 2")
    "TAB 3" '((lambda () (interactive) (tab-bar-select-tab 3)) :which-key "tab 3")
    "TAB 4" '((lambda () (interactive) (tab-bar-select-tab 4)) :which-key "tab 4")
    "TAB 5" '((lambda () (interactive) (tab-bar-select-tab 5)) :which-key "tab 5")
    "TAB `" '(tab-bar-switch-to-recent-tab :which-key "last tab"))

  (tabspaces-mode 1))


;;; ============================================================
;;; Org-Mode
;;; ============================================================

(use-package org
  :ensure nil                            ; built-in, don't install from MELPA
  :config
  ;; --- Directories ---
  (setq org-directory "~/org/")

  ;; --- Indentation ---
  ;; Virtual indentation based on heading level (no extra spaces in file).
  (setq org-startup-indented t
        org-hide-leading-stars t)

  ;; --- Rendered-like display ---
  ;; Show inline images, render \alpha → α, style quote blocks, use a
  ;; chevron fold indicator instead of ellipsis.
  (setq org-startup-with-inline-images t
        org-image-actual-width '(600)
        org-pretty-entities t
        org-pretty-entities-include-sub-superscripts nil
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-ellipsis " ▾"
        org-link-descriptive t)

  ;; Word wrap — wrap at word boundaries, respecting org-indent.
  (add-hook 'org-mode-hook #'visual-line-mode)

  ;; --- TODO Keywords ---
  (setq org-todo-keywords
        '((sequence "TODO(t)" "|" "DONE(d)")))

  ;; --- Logging ---
  ;; Timestamp when marking DONE, store in LOGBOOK drawer.
  (setq org-log-done 'time
        org-log-into-drawer t)

  ;; --- Auto-save ---
  ;; Save org buffers periodically (protects against data loss).
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local auto-save-visited-interval 10)
              (auto-save-visited-mode 1)))

  ;; --- Attachments ---
  ;; Matches Doom config: timestamp-based folders under ~/org/attachments/
  (setq org-id-method 'ts
        org-attach-id-to-path-function-list
        '(org-attach-id-ts-folder-format
          org-attach-id-uuid-folder-format)
        org-attach-directory (expand-file-name "attachments/" org-directory)
        org-attach-method 'cp
        org-attach-store-link-p 'attached
        org-attach-use-inheritance t
        org-attach-dir-relative t
        org-attach-preferred-new-method 'dir
        org-attach-archive-delete 'query)

  ;; --- Archiving ---
  ;; Default fallback: archive to ./archive/<filename> under a datetree.
  ;; Per-heading :ARCHIVE: properties in todo.org override this (e.g.
  ;; archive/enablement.org, archive/interrupt.org, etc.)
  (setq org-archive-location "archive/%s::")

  ;; --- Link navigation ---
  (defun thb/org-open-link-other-window ()
    "Open org link at point in another window."
    (interactive)
    (let ((org-link-frame-setup
           (cl-acons 'file #'find-file-other-window org-link-frame-setup)))
      (org-open-at-point)))

  ;; --- Table cell inspector ---
  ;; In wide/shrunk tables (e.g. columnview dblocks with valign), show the
  ;; full content of the cell at point as a posframe at point.  This is a
  ;; child-frame popup — it draws on top of the buffer without shifting any
  ;; text.  Dismissed automatically by any next command.  Falls back to
  ;; momentary-string-display in terminal frames.  Bound to C-c TAB,
  ;; replacing the default `org-table-toggle-column-width' behavior.
  (defun thb/org-table-show-cell ()
    "Show full content of the current org-table cell as a posframe popup."
    (interactive)
    (if (not (org-at-table-p))
        (user-error "Not in a table")
      (let* ((content (string-trim (org-table-get-field)))
             (col (org-table-current-column))
             (col-name (save-excursion
                         (goto-char (org-table-begin))
                         (while (or (looking-at "^[ \t]*|[-+]")
                                    (looking-at "^[ \t]*|[ \t]*<[0-9]+>[ \t]*|"))
                           (forward-line))
                         (string-trim (org-table-get-field col))))
             (display (concat (propertize (format "[%s] " col-name) 'face 'shadow)
                              (propertize content 'face 'highlight))))
        (cond
         ((string-empty-p content)
          (message "(empty cell)"))
         ((and (display-graphic-p) (require 'posframe nil t))
          (posframe-show " *thb-cell-popup*"
                         :string display
                         :position (point)
                         :internal-border-width 8
                         :internal-border-color (face-attribute 'mode-line :foreground)
                         :background-color (face-attribute 'tooltip :background nil 'default)
                         :foreground-color (face-attribute 'tooltip :foreground nil 'default)
                         :max-width (- (frame-width) 4))
          ;; Hide on next command (keystroke or motion).
          (add-hook 'post-command-hook #'thb/org-table-hide-cell-popup))
         (t
          (momentary-string-display
           (concat "\n  ↳ " display "\n")
           (line-end-position)))))))

  (defun thb/org-table-hide-cell-popup ()
    "Internal: hide the table cell popup on next command."
    (unless (eq this-command 'thb/org-table-show-cell)
      (when (and (fboundp 'posframe-hide)
                 (get-buffer " *thb-cell-popup*"))
        (posframe-hide " *thb-cell-popup*"))
      (remove-hook 'post-command-hook #'thb/org-table-hide-cell-popup)))

  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c TAB") #'thb/org-table-show-cell))

  (thb/leader
    :keymaps 'org-mode-map
    "m"  '(:ignore t :which-key "org")
    "mt" '(org-todo :which-key "todo state")
    "mq" '(org-set-tags-command :which-key "set tags")
    "mo" '(org-set-property :which-key "set property")
    "me" '(org-export-dispatch :which-key "export")
    "mx" '(org-toggle-checkbox :which-key "toggle checkbox")
    "m." '(consult-org-heading :which-key "goto heading")

    ;; --- SPC m d: date/deadline ---
    "md"  '(:ignore t :which-key "date/deadline")
    "mdd" '(org-deadline :which-key "deadline")
    "mds" '(org-schedule :which-key "schedule")
    "mdt" '(org-time-stamp :which-key "timestamp")
    "mdT" '(org-time-stamp-inactive :which-key "inactive timestamp")

    ;; --- SPC m s: tree/subtree ---
    "ms"  '(:ignore t :which-key "subtree")
    "msj" '(org-move-subtree-down :which-key "move down")
    "msk" '(org-move-subtree-up :which-key "move up")
    "msh" '(org-promote-subtree :which-key "promote")
    "msl" '(org-demote-subtree :which-key "demote")
    "msn" '(org-narrow-to-subtree :which-key "narrow")
    "msN" '(widen :which-key "widen")
    "mss" '(org-sparse-tree :which-key "sparse tree")
    "msS" '(org-sort :which-key "sort")
    "msd" '(org-cut-subtree :which-key "cut subtree")
    "msb" '(org-tree-to-indirect-buffer :which-key "indirect buffer")
    "msa" '(org-toggle-archive-tag :which-key "toggle archive tag")
    "msA" '(org-archive-subtree-default :which-key "archive subtree")

    ;; --- SPC m l: links ---
    "ml"  '(:ignore t :which-key "links")
    "mlo" '(org-open-at-point :which-key "open link")
    "mls" '(thb/org-open-link-other-window :which-key "open in split")
    "mll" '(org-insert-link :which-key "insert link")
    "mln" '(org-store-link :which-key "store link")
    "mlt" '(org-toggle-link-display :which-key "toggle display")

    ;; --- SPC m r: refile ---
    "mr"  '(:ignore t :which-key "refile")
    "mrr" '(org-refile :which-key "refile")

    ;; --- SPC m p: priority ---
    "mp"  '(:ignore t :which-key "priority")
    "mpp" '(org-priority :which-key "set priority")
    "mpu" '(org-priority-up :which-key "priority up")
    "mpd" '(org-priority-down :which-key "priority down")

    ;; --- SPC m v: preview (rendered view) ---
    "mv"  '(:ignore t :which-key "preview")
    "mvh" '(org-preview-html-mode :which-key "html (eww)")
    "mvm" '(org-markdown-preview :which-key "markdown (browser)")
    "mvf" '(org-fragtog-mode :which-key "fragtog (latex)")))


;;; ============================================================
;;; Org-Roam (database layer — used by Vulpea)
;;; ============================================================

;; org-roam provides the SQLite database that Vulpea builds on.
;; No direct org-roam UI config — Vulpea is the primary interface.
(use-package org-roam
  :after org
  :config
  (setq org-roam-directory (expand-file-name "roam/" org-directory)))


;;; ============================================================
;;; Vulpea (primary note interface)
;;; ============================================================

;; Vulpea v2 — primary note interface built on org-roam-db with async indexing.
(use-package vulpea
  :after org
  :config
  ;; Index both roam notes and daily journal entries.
  (setq vulpea-db-sync-directories
        (list (expand-file-name "roam/" org-directory)
              (expand-file-name "daily/" org-directory))
        vulpea-default-notes-directory
        (expand-file-name org-directory)

        ;; Performance settings from d12frosted/environment:
        vulpea-db-parse-method         'single-temp-buffer  ; reuse temp buffer
        vulpea-db-sync-scan-on-enable  'async               ; non-blocking startup
        vulpea-db-sync-external-method 'fswatch             ; macOS: detect git changes
        vulpea-db-index-heading-level  t)                   ; index headings with IDs

  ;; Sort vulpea-find candidates by file modification time (most recent first).
  (setq vulpea-find-default-candidates-source
        (lambda (&optional filter-fn)
          (sort (vulpea-db-query filter-fn)
                (lambda (a b)
                  (time-less-p
                   (file-attribute-modification-time
                    (file-attributes (vulpea-note-path b)))
                   (file-attribute-modification-time
                    (file-attributes (vulpea-note-path a))))))))

  ;; Disable vertico's re-sorting for vulpea-find so mtime order is preserved.
  (define-advice vulpea-find (:around (fn &rest args) preserve-mtime-order)
    (let ((vertico-sort-function nil))
      (apply fn args)))

  (setq vulpea-create-default-template
        '(:file-name "roam/%<%Y%m%d%H%M>.org"
          :head "#+date: %<[%Y-%m-%d]>"
          :properties (("CREATED" . "%<[%Y-%m-%d %a %H:%M]>"))
          :meta (("status" . "in-progress"))
          :tags nil))

  ;; Guard against fswatch process leak: kill existing process before
  ;; spawning a new one (upstream bug — setup-fswatch overwrites the
  ;; variable without cleanup).  Clear the sentinel first so the
  ;; upstream sentinel doesn't schedule a competing restart via
  ;; run-at-time, which creates an infinite kill/restart loop.
  (define-advice vulpea-db-sync--setup-fswatch (:before (&rest _) kill-stale)
    (when (and vulpea-db-sync--fswatch-process
               (process-live-p vulpea-db-sync--fswatch-process))
      (set-process-sentinel vulpea-db-sync--fswatch-process #'ignore)
      (delete-process vulpea-db-sync--fswatch-process)
      (setq vulpea-db-sync--fswatch-process nil)))

  (vulpea-db-autosync-mode 1))

;; Vulpea-UI — sidebar with outline, backlinks, stats widgets.
;; Loaded but not auto-opened; use SPC n S to toggle manually.
(use-package vulpea-ui
  :after vulpea)

;; Vulpea-journal — daily journaling with calendar sidebar widget.
;; Template matches existing daily format in ~/org/daily/.
(use-package vulpea-journal
  :after (vulpea vulpea-ui)
  :config
  (setq vulpea-journal-default-template
        '(:file-name "daily/%Y-%m-%d.org"
          :title "%Y-%m-%d %A"
          :tags ("daily")
          :properties (("CREATED" . "%<[%Y-%m-%d]>"))))

  ;; Don't auto-open sidebar when visiting journal; use SPC n S to toggle.
  (defun vulpea-journal (&optional date)
    "Open journal note for DATE (defaults to today)."
    (interactive)
    (vulpea-visit (vulpea-journal-note (or date (current-time)))))

  (vulpea-journal-setup))

;; Consult-vulpea — search vulpea notes with consult/ripgrep.
(use-package consult-vulpea
  :after vulpea
  :config
  (setq consult-vulpea-grep-func #'consult-ripgrep))



;;; ============================================================
;;; Org-Modern
;;; ============================================================

;; Visual improvements for org-mode: pretty headings, lists, tables, TODOs.
(use-package org-modern
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :config
  (setq org-modern-star 'replace
        org-modern-replace-stars "◉○✸✿✤✜✢❉"
        org-modern-hide-stars 'leading
        org-modern-table-vertical 1
        org-modern-table-horizontal 0.2
        org-modern-list '((?* . "◦") (?+ . "•") (?- . "–"))
        org-modern-checkbox '((?X . "☑")
                              (?- . "◐")
                              (?\s . "☐"))
        org-modern-priority
        '((?A . "❗")
          (?B . "⬆")
          (?C . "⬇"))
        org-modern-todo-faces
        '(("TODO" :inverse-video t :inherit org-todo)
          ("DONE" :inverse-video t :inherit org-done))
        org-modern-fold-stars
        '(("▸" . "▾")
          ("▹" . "▿")
          ("▷" . "▽"))
        org-modern-progress 4
        org-modern-keyword t
        org-modern-block-fringe nil
        org-modern-block-name '((t . t)
                                ("src" "»" "«")
                                ("example" "»–" "–«")
                                ("quote" "❝" "❞"))
        org-modern-todo t))

;; Face customizations for org-modern, headings, and source blocks
;; are in thb/apply-org-faces (Appearance section) — do not duplicate here.

;; Org-modern-indent — block borders that follow `org-indent` virtual indentation.
;; Installed and available, but not auto-hooked: the inline bars duplicate the
;; visual role of org-block backgrounds. Toggle manually if needed.
(use-package org-modern-indent
  :vc (:url "https://github.com/jdtsmith/org-modern-indent" :rev :newest)
  :commands org-modern-indent-mode)

;; Org-appear — hide markup until cursor enters (~code~, *bold*, etc.)
;; Manual trigger: only expand links/emphasis in evil insert mode.
(use-package org-appear
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-autokeywords t
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

;; Valign — pixel-perfect alignment of org tables, including those with
;; unicode characters or ligatures (PragmataPro arrows, →, etc.).
(use-package valign
  :hook (org-mode . valign-mode)
  :config
  ;; Plain bars; fancy bars render hline rows as fragmented "+" segments
  ;; that look broken when columns are shrunk.
  (setq valign-fancy-bar nil))

;; Org-fragtog — auto-toggle LaTeX fragment previews on cursor enter/leave.
;; Pairs with org-pretty-entities for rendered-like math display.
(use-package org-fragtog
  :hook (org-mode . org-fragtog-mode))


;;; ============================================================
;;; Org Preview (browser-rendered views)
;;; ============================================================

;; Org-preview-html — refresh-on-save HTML preview in `eww` (or xwidget).
;; Lightweight: uses Emacs's built-in browser, no external process.
(use-package org-preview-html
  :commands (org-preview-html-mode org-preview-html)
  :config
  (setq org-preview-html-viewer 'eww
        org-preview-html-refresh-configuration 'save))

;; Org-markdown-preview — Pandoc-driven realtime preview of org as markdown
;; rendered to HTML in an external browser. Useful for review-style reading.
;; Not on MELPA — installed via :vc from upstream.
(use-package org-markdown-preview
  :vc (:url "https://github.com/KarimAziev/org-markdown-preview" :rev :newest)
  :commands org-markdown-preview)


;;; ============================================================
;;; Go
;;; ============================================================

(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . eglot-ensure)
  :config
  (with-eval-after-load 'eglot
    (setq-default eglot-workspace-configuration
                  '(:gopls (:directoryFilters ["-bazel-bin"
                                               "-bazel-out"
                                               "-bazel-testlogs"])))))
;; Monorepo / vendor-specific gopls customizations (alternate server
;; binary, :local imports prefix, extra build flags / directory filters,
;; GOPLS_DISABLE_MODULE_LOADS) live in local.el so they don't leak into
;; the public dotfiles.


;;; ============================================================
;;; Markdown — tree-sitter
;;; ============================================================
;;
;; Phase 1: bare switch from `markdown-mode' to `markdown-ts-mode'.
;;
;; The MELPA `markdown-ts-mode' (v0.3.0, by Rahul M. Juliato — soon to be
;; superseded by an Emacs 31 built-in version) provides only fontification
;; and iMenu over a tree-sitter parse. It exposes generic faces
;; (`font-lock-keyword-face' for headings, `font-lock-string-face' for code,
;; `bold' / `underline' / `link' for emphasis) and ships no keymap or
;; editing commands. Display customisation (header scaling, code/quote
;; backgrounds, checkbox prettification) and an org-style `SPC m' leader
;; will be rebuilt on top of this in phase 2.
;;
;; Requires BOTH the `markdown' and `markdown-inline' tree-sitter grammars;
;; `thb/markdown-ensure-grammar' installs them on first load if missing.

(use-package markdown-ts-mode
  :mode (("\\.md\\'" . markdown-ts-mode)
         ("\\.markdown\\'" . markdown-ts-mode))
  :defer t
  :init
  (with-eval-after-load 'treesit
    (dolist (entry '((markdown
                      "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                      "split_parser" "tree-sitter-markdown/src")
                     (markdown-inline
                      "https://github.com/tree-sitter-grammars/tree-sitter-markdown"
                      "split_parser" "tree-sitter-markdown-inline/src")))
      (add-to-list 'treesit-language-source-alist entry)))

  (defun thb/markdown-ensure-grammar ()
    "Install markdown + markdown-inline tree-sitter grammars if missing."
    (dolist (lang '(markdown markdown-inline))
      (unless (treesit-language-available-p lang)
        (message "markdown-ts-mode: installing tree-sitter grammar %s" lang)
        (treesit-install-language-grammar lang))))

  :config
  (thb/markdown-ensure-grammar)

  ;; --- Phase 2: faces + heading scaling ---------------------------------
  ;;
  ;; The MELPA package collapses all heading levels to `font-lock-keyword-face'
  ;; and uses generic faces for code/emphasis/links.  We replace its tree-sitter
  ;; query rules with a richer set that captures each heading level separately,
  ;; subdues syntactic markers, and routes blocks/inlines through semantic
  ;; faces we own.  Capture names in tree-sitter queries cannot contain `/',
  ;; so these faces use a `thb-' prefix instead of `thb/'.

  (defgroup thb-markdown-ts nil
    "Display customization for `markdown-ts-mode'."
    :group 'markdown)

  (defface thb-markdown-ts-h1 '((t :inherit outline-1 :weight bold :height 1.3))
    "Face for level-1 atx & setext headings." :group 'thb-markdown-ts)
  (defface thb-markdown-ts-h2 '((t :inherit outline-2 :weight bold :height 1.15))
    "Face for level-2 atx & setext headings." :group 'thb-markdown-ts)
  (defface thb-markdown-ts-h3 '((t :inherit outline-3 :weight semi-bold :height 1.05))
    "Face for level-3 atx headings." :group 'thb-markdown-ts)
  (defface thb-markdown-ts-h4 '((t :inherit outline-4 :weight semi-bold))
    "Face for level-4 atx headings." :group 'thb-markdown-ts)
  (defface thb-markdown-ts-h5 '((t :inherit outline-5 :weight semi-bold))
    "Face for level-5 atx headings." :group 'thb-markdown-ts)
  (defface thb-markdown-ts-h6 '((t :inherit outline-6 :weight semi-bold))
    "Face for level-6 atx headings." :group 'thb-markdown-ts)
  (defface thb-markdown-ts-marker '((t :inherit shadow))
    "Face for syntactic markers (#, *, -, `, [], etc.)." :group 'thb-markdown-ts)
  (defface thb-markdown-ts-code-inline '((t :inherit (font-lock-string-face fixed-pitch)))
    "Face for inline code spans." :group 'thb-markdown-ts)
  (defface thb-markdown-ts-code-block '((t :inherit (font-lock-string-face fixed-pitch) :extend t))
    "Face for fenced/indented code-block content." :group 'thb-markdown-ts)
  (defface thb-markdown-ts-fence-delim '((t :inherit (shadow fixed-pitch)))
    "Face for fenced code-block delimiters." :group 'thb-markdown-ts)
  (defface thb-markdown-ts-info-language '((t :inherit font-lock-type-face))
    "Face for language tag in fenced code-block info string." :group 'thb-markdown-ts)
  (defface thb-markdown-ts-blockquote '((t :slant italic :extend t))
    "Face for block quotes." :group 'thb-markdown-ts)
  (defface thb-markdown-ts-emphasis '((t :slant italic))
    "Face for emphasis (italic)." :group 'thb-markdown-ts)
  (defface thb-markdown-ts-strong '((t :weight bold))
    "Face for strong emphasis (bold)." :group 'thb-markdown-ts)
  (defface thb-markdown-ts-strikethrough '((t :strike-through t))
    "Face for strikethrough text." :group 'thb-markdown-ts)
  (defface thb-markdown-ts-link '((t :inherit link))
    "Face for link text and image descriptions." :group 'thb-markdown-ts)
  (defface thb-markdown-ts-url '((t :inherit (link shadow)))
    "Face for link destinations." :group 'thb-markdown-ts)

  (defun thb-markdown-ts--rules ()
    "Build the replacement `treesit-font-lock-rules' for `markdown-ts-mode'."
    (treesit-font-lock-rules
     :language 'markdown-inline
     :override t
     :feature 'delimiter
     '([ "[" "]" "(" ")" ] @thb-markdown-ts-marker)

     :language 'markdown
     :override t
     :feature 'paragraph
     '(;; ATX headings — scale text, dim markers.
       (atx_heading (atx_h1_marker) @thb-markdown-ts-marker
                    heading_content: (inline) @thb-markdown-ts-h1)
       (atx_heading (atx_h2_marker) @thb-markdown-ts-marker
                    heading_content: (inline) @thb-markdown-ts-h2)
       (atx_heading (atx_h3_marker) @thb-markdown-ts-marker
                    heading_content: (inline) @thb-markdown-ts-h3)
       (atx_heading (atx_h4_marker) @thb-markdown-ts-marker
                    heading_content: (inline) @thb-markdown-ts-h4)
       (atx_heading (atx_h5_marker) @thb-markdown-ts-marker
                    heading_content: (inline) @thb-markdown-ts-h5)
       (atx_heading (atx_h6_marker) @thb-markdown-ts-marker
                    heading_content: (inline) @thb-markdown-ts-h6)
       ;; Setext headings (=== and --- underlines).
       (setext_heading heading_content: (paragraph) @thb-markdown-ts-h1
                       (setext_h1_underline) @thb-markdown-ts-marker)
       (setext_heading heading_content: (paragraph) @thb-markdown-ts-h2
                       (setext_h2_underline) @thb-markdown-ts-marker)
       ;; Code blocks.
       (fenced_code_block (fenced_code_block_delimiter) @thb-markdown-ts-fence-delim)
       (fenced_code_block (info_string (language) @thb-markdown-ts-info-language))
       (fenced_code_block (code_fence_content) @thb-markdown-ts-code-block)
       (indented_code_block) @thb-markdown-ts-code-block
       ;; Lists and tasks.
       (list_item (list_marker_minus) @thb-markdown-ts-marker)
       (list_item (list_marker_plus) @thb-markdown-ts-marker)
       (list_item (list_marker_star) @thb-markdown-ts-marker)
       (list_item (list_marker_dot) @thb-markdown-ts-marker)
       (list_item (task_list_marker_unchecked) @thb-markdown-ts-marker)
       (list_item (task_list_marker_checked) @thb-markdown-ts-marker)
       ;; Blockquotes and horizontal rules.
       (block_quote (block_quote_marker) @thb-markdown-ts-marker)
       (block_quote (paragraph) @thb-markdown-ts-blockquote)
       (thematic_break) @thb-markdown-ts-marker)

     :language 'markdown-inline
     :override t
     :feature 'paragraph-inline
     '((emphasis) @thb-markdown-ts-emphasis
       (strong_emphasis) @thb-markdown-ts-strong
       (strikethrough) @thb-markdown-ts-strikethrough
       (emphasis_delimiter) @thb-markdown-ts-marker
       (code_span) @thb-markdown-ts-code-inline
       (code_span_delimiter) @thb-markdown-ts-marker
       (inline_link (link_text) @thb-markdown-ts-link
                    (link_destination) @thb-markdown-ts-url)
       (image (image_description) @thb-markdown-ts-link
              (link_destination) @thb-markdown-ts-url)
       (shortcut_link (link_text) @thb-markdown-ts-link))))

  (defun thb/apply-markdown-ts-faces ()
    "Reapply theme-aware background colors to `thb-markdown-ts-*' faces.
Called from `thb/toggle-theme' so colors track light/dark swaps.

Note: no `display-graphic-p' guard — hex colors set on face attributes
work in any frame type (terminal Emacs just approximates), and gating
on the selected frame breaks `with-eval-after-load' / emacsclient calls
that happen before the GUI frame is the selected one."
    (when (featurep 'modus-themes)
      (let ((bg-code  (modus-themes-get-color-value 'bg-dim))
            (bg-quote (modus-themes-get-color-value 'bg-blue-nuanced)))
        (set-face-attribute 'thb-markdown-ts-code-block nil :background bg-code)
        (set-face-attribute 'thb-markdown-ts-blockquote nil :background bg-quote))))

  (with-eval-after-load 'modus-themes
    (thb/apply-markdown-ts-faces))

  ;; --- Phase 3: checkbox prettify ----------------------------------------
  ;;
  ;; Render Markdown task-list checkboxes as Unicode glyphs via
  ;; `prettify-symbols-mode'.  Pattern-based (not semantic) — a literal
  ;; `- [ ]' inside a code block or blockquote will also render as ☐, which
  ;; matches the rendered-Markdown intent.  Composes with PragmataPro
  ;; ligatures (ligatures handle prog-mode glyphs; this handles markdown
  ;; checkbox tokens).

  (defvar thb-markdown-ts-prettify-symbols
    '(("- [ ]" . ?☐)
      ("- [x]" . ?☑)
      ("- [X]" . ?☑)
      ("- [-]" . ?◐)
      ("* [ ]" . ?☐)
      ("* [x]" . ?☑)
      ("* [X]" . ?☑)
      ("* [-]" . ?◐)
      ("+ [ ]" . ?☐)
      ("+ [x]" . ?☑)
      ("+ [X]" . ?☑)
      ("+ [-]" . ?◐))
    "Markdown task-list checkbox tokens shown as Unicode glyphs.
☐ unchecked  ☑ checked  ◐ in-progress (- [-] is a common GFM-ish convention).")

  (defun thb/markdown-setup ()
    "Per-buffer markdown setup: visual line wrapping and custom font-lock rules.

Replaces `markdown-ts--treesit-settings' with our richer rule set so each
heading level, markers, code, blockquotes, and inlines get their own faces.

Also installs `treesit-range-settings' restricting the markdown-inline
parser to (inline) nodes from the markdown grammar. Without this, the
MELPA package runs both parsers across the whole buffer and the inline
parser misclassifies fenced code-block content as `code_span', stealing
fontification from the block-level rules."
    (visual-line-mode 1)
    (setq-local treesit-range-settings
                (treesit-range-rules
                 :embed 'markdown-inline
                 :host 'markdown
                 '((inline) @capture)))
    (setq-local treesit-font-lock-settings (thb-markdown-ts--rules))
    (when (fboundp 'treesit-font-lock-recompute-features)
      (treesit-font-lock-recompute-features))
    ;; Prettify checkboxes.  Append to (don't replace) prettify-symbols-alist
    ;; so any future additions from other minor modes still work.
    (setq-local prettify-symbols-alist
                (append thb-markdown-ts-prettify-symbols
                        prettify-symbols-alist))
    (prettify-symbols-mode 1)
    (when (treesit-parser-list)
      (treesit-font-lock-fontify-region (point-min) (point-max))))
  (add-hook 'markdown-ts-mode-hook #'thb/markdown-setup)

  ;; --- Phase 4: editing commands + SPC m leader -------------------------
  ;;
  ;; `markdown-ts-mode' v0.3 ships zero editing commands and no keymap.
  ;; We implement org-style heading / subtree / link manipulation directly
  ;; against the tree-sitter parse, then bind via the existing `thb/leader'.
  ;; All helpers and commands use a `thb-markdown-ts-' prefix.

  (defun thb-markdown-ts--parent-of-type (pt type &optional lang)
    "Find nearest ancestor of TYPE (string) at PT in LANG (default `markdown')."
    (when-let* ((lang (or lang 'markdown))
                (node (treesit-node-at pt lang)))
      (treesit-parent-until
       node
       (lambda (n) (equal (treesit-node-type n) type))
       t)))

  (defun thb-markdown-ts--atx-heading-at-point ()
    "Return the `atx_heading' node containing point, or nil."
    (thb-markdown-ts--parent-of-type (point) "atx_heading"))

  (defun thb-markdown-ts--section-at-point ()
    "Return the innermost `section' node containing point, or nil.
A section spans an ATX heading plus all its content/nested sections."
    (thb-markdown-ts--parent-of-type (point) "section"))

  (defun thb-markdown-ts--task-list-marker-at-point ()
    "Return the `task_list_marker_*' node on the line at point, or nil.
Walks the `list_item' ancestor and returns its task-marker child if any."
    (when-let* ((li (thb-markdown-ts--parent-of-type (point) "list_item")))
      (seq-find (lambda (c)
                  (string-prefix-p "task_list_marker_"
                                   (treesit-node-type c)))
                (treesit-node-children li))))

  (defun thb-markdown-ts-promote-heading ()
    "Promote heading at point one level (e.g. ## → #).  Errors at level 1."
    (interactive)
    (let ((h (thb-markdown-ts--atx-heading-at-point)))
      (unless h (user-error "Not on an ATX heading"))
      (let* ((marker (treesit-node-child h 0))
             (start  (treesit-node-start marker))
             (end    (treesit-node-end marker))
             (level  (- end start)))
        (when (<= level 1) (user-error "Already at level 1"))
        (save-excursion (goto-char start) (delete-char 1)))))

  (defun thb-markdown-ts-demote-heading ()
    "Demote heading at point one level (e.g. # → ##).  Errors at level 6."
    (interactive)
    (let ((h (thb-markdown-ts--atx-heading-at-point)))
      (unless h (user-error "Not on an ATX heading"))
      (let* ((marker (treesit-node-child h 0))
             (start  (treesit-node-start marker))
             (end    (treesit-node-end marker))
             (level  (- end start)))
        (when (>= level 6) (user-error "Already at level 6"))
        (save-excursion (goto-char start) (insert "#")))))

  (defun thb-markdown-ts--swap-with-sibling (direction)
    "Swap the section at point with its sibling in DIRECTION (`next' or `prev').
Preserves point's offset within the section so the caret stays on the
same logical line after the move."
    (let ((section (thb-markdown-ts--section-at-point)))
      (unless section (user-error "Not in a section"))
      (let* ((sibling (pcase direction
                        ('next (treesit-node-next-sibling section))
                        ('prev (treesit-node-prev-sibling section))))
             (_ (unless (and sibling
                             (equal (treesit-node-type sibling) "section"))
                  (user-error "No %s section to swap with"
                              (if (eq direction 'next) "following" "preceding"))))
             (s-start (treesit-node-start section))
             (s-end   (treesit-node-end   section))
             (b-start (treesit-node-start sibling))
             (b-end   (treesit-node-end   sibling))
             (offset  (- (point) s-start))
             (s-text  (buffer-substring s-start s-end))
             (b-text  (buffer-substring b-start b-end)))
        (cond
         ((eq direction 'next)
          (delete-region s-start b-end)
          (goto-char s-start)
          (insert b-text s-text)
          ;; New section start = old start + length(sibling text).
          (goto-char (+ s-start (length b-text) offset)))
         ((eq direction 'prev)
          (delete-region b-start s-end)
          (goto-char b-start)
          (insert s-text b-text)
          (goto-char (+ b-start offset)))))))

  (defun thb-markdown-ts-move-subtree-down ()
    "Move section (heading + content) at point down past its next sibling."
    (interactive)
    (thb-markdown-ts--swap-with-sibling 'next))

  (defun thb-markdown-ts-move-subtree-up ()
    "Move section (heading + content) at point up past its previous sibling."
    (interactive)
    (thb-markdown-ts--swap-with-sibling 'prev))

  (defun thb-markdown-ts-narrow-to-subtree ()
    "Narrow buffer to the section (heading + content) at point."
    (interactive)
    (let ((section (thb-markdown-ts--section-at-point)))
      (unless section (user-error "Not in a section"))
      (narrow-to-region (treesit-node-start section)
                        (treesit-node-end section))))

  (defun thb-markdown-ts-cut-subtree ()
    "Kill the section (heading + content) at point."
    (interactive)
    (let ((section (thb-markdown-ts--section-at-point)))
      (unless section (user-error "Not in a section"))
      (kill-region (treesit-node-start section)
                   (treesit-node-end section))))

  (defun thb-markdown-ts-toggle-checkbox ()
    "Toggle the task-list checkbox on the line at point ([ ] ↔ [x])."
    (interactive)
    (let ((marker (thb-markdown-ts--task-list-marker-at-point)))
      (unless marker (user-error "No task-list checkbox on this line"))
      (let* ((start (treesit-node-start marker))
             (end   (treesit-node-end   marker))
             (text  (buffer-substring-no-properties start end))
             (new   (cond
                     ((string-match-p "\\[ \\]"   text) "[x]")
                     ((string-match-p "\\[[xX]\\]" text) "[ ]")
                     ((string-match-p "\\[-\\]"   text) "[ ]")
                     (t text))))
        (save-excursion
          (goto-char start)
          (delete-region start end)
          (insert new)))))

  (defun thb-markdown-ts--link-at-point ()
    "Return the enclosing `inline_link', `image', or `shortcut_link' node.
Uses the markdown-inline parser; returns nil when point is outside any
inline region (e.g. inside a fenced code block)."
    (when-let* ((node (treesit-node-at (point) 'markdown-inline)))
      (treesit-parent-until
       node
       (lambda (n) (member (treesit-node-type n)
                           '("inline_link" "image" "shortcut_link")))
       t)))

  (defun thb-markdown-ts-open-link ()
    "Open the markdown link at point via `browse-url'."
    (interactive)
    (let ((link (thb-markdown-ts--link-at-point)))
      (unless link (user-error "Not on a link"))
      (let ((dest (seq-find
                   (lambda (c) (equal (treesit-node-type c) "link_destination"))
                   (treesit-node-children link))))
        (unless dest (user-error "Link has no destination"))
        (browse-url (treesit-node-text dest t)))))

  (defun thb-markdown-ts-insert-link (text url)
    "Insert a markdown inline link [TEXT](URL).
With an active region, TEXT defaults to the region's content."
    (interactive
     (let* ((default (when (use-region-p)
                       (buffer-substring-no-properties
                        (region-beginning) (region-end))))
            (text (read-string (format-prompt "Link text" default)
                               nil nil default))
            (url  (read-string "URL: ")))
       (list text url)))
    (when (use-region-p) (delete-region (region-beginning) (region-end)))
    (insert (format "[%s](%s)" text url)))

  ;; --- SPC m leader bindings -------------------------------------------
  (thb/leader
    :keymaps 'markdown-ts-mode-map
    "m"   '(:ignore t :which-key "markdown")
    "mx"  '(thb-markdown-ts-toggle-checkbox :which-key "toggle checkbox")
    "m."  '(consult-imenu               :which-key "goto heading")

    ;; --- SPC m s: subtree ---
    "ms"  '(:ignore t :which-key "subtree")
    "msh" '(thb-markdown-ts-promote-heading   :which-key "promote")
    "msl" '(thb-markdown-ts-demote-heading    :which-key "demote")
    "msj" '(thb-markdown-ts-move-subtree-down :which-key "move down")
    "msk" '(thb-markdown-ts-move-subtree-up   :which-key "move up")
    "msn" '(thb-markdown-ts-narrow-to-subtree :which-key "narrow")
    "msN" '(widen                              :which-key "widen")
    "msd" '(thb-markdown-ts-cut-subtree        :which-key "cut")

    ;; --- SPC m l: links ---
    "ml"  '(:ignore t :which-key "links")
    "mlo" '(thb-markdown-ts-open-link   :which-key "open at point")
    "mll" '(thb-markdown-ts-insert-link :which-key "insert link")))


;;; ============================================================
;;; Beads (Issue Tracking)
;;; ============================================================

(use-package hierarchy)

(use-package beads
  :ensure nil
  :load-path "~/src/beads.el/lisp"
  :commands (beads beads-project-list beads-create-issue beads-activity beads-stats)
  :config
  ;; Pin to br backend.  Auto-detect prefers bd when both are on PATH, but
  ;; bd >=0.58 dropped the `daemon' subcommand, so the bd-backend daemon
  ;; start fails with exit code 1 and the viewer dies on launch.  br has
  ;; no daemon support and beads-client falls back to direct CLI.
  (setq beads-cli-program "br"
        beads-autoupdate-enable t
        beads-list-highlight-p0-rows t
        beads-detail-render-markdown t
        beads-verbose t)

  ;; Silently skip autoupdate when no .beads/ is reachable from the buffer's
  ;; default-directory.  Prevents recurring "No Beads database found" errors
  ;; in list buffers opened from a non-project path (e.g. ~/org/roam/).
  (with-eval-after-load 'beads-autoupdate
    (define-advice beads-autoupdate--refresh (:around (orig &rest args) skip-no-db)
      (when (ignore-errors (beads-client--find-database))
        (apply orig args)))))

;;; ============================================================
;;; Version Control + Local Overrides
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
  "Save current session and restart Emacs, restoring open buffers.
In daemon mode, restarts as a daemon."
  (interactive)
  (let ((dir (expand-file-name "restart/" user-emacs-directory)))
    (make-directory dir t)
    (desktop-save dir t)
    (if (daemonp)
        (restart-emacs (format "--daemon=%s" (daemonp)))
      (restart-emacs))))

;; Restore session after restart (runs once, then cleans up)
(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((dir (expand-file-name "restart/" user-emacs-directory))
                  (file "restart/.emacs.desktop"))
              (when (file-exists-p (expand-file-name file user-emacs-directory))
                (desktop-read dir)
                (delete-file (expand-file-name ".emacs.desktop" dir) t)
                (delete-file (expand-file-name ".emacs.desktop.lock" dir) t)))))

;; --- Local Overrides ---
;; Load machine-specific settings from local.el (not tracked in git).
;; Copy local.el.example to local.el and customize.
(let ((local-config (expand-file-name "local.el" user-emacs-directory)))
  (when (file-exists-p local-config)
    (load local-config 'noerror 'nomessage)))


;;; init.el ends here
