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

;; Local lisp under thbemacs/lisp/ — homegrown modules live here.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))



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
               thb-md-render-mode-hook  ; derived from special-mode but
                                        ; define-derived-mode doesn't run
                                        ; parent hooks, so list explicitly
               special-mode-hook
               term-mode-hook
               eshell-mode-hook
               eww-mode-hook))
  (add-hook mode #'thb/disable-line-numbers))

;; Disable left + right fringes for prose modes (org, markdown-ts).  Both
;; modes are read-and-edit text where fringes add visual gutter without
;; carrying useful information.  We keep fringes everywhere else so things
;; like diff-hl / flycheck indicators still have somewhere to render.
(defun thb/disable-fringes ()
  "Remove left and right fringes in the current buffer."
  (setq-local left-fringe-width 0)
  (setq-local right-fringe-width 0)
  (when-let ((win (get-buffer-window (current-buffer))))
    (set-window-fringes win 0 0)))
(dolist (mode '(org-mode-hook
               markdown-ts-mode-hook
               thb-md-render-mode-hook))
  (add-hook mode #'thb/disable-fringes))

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

;; variable-pitch — IBM Plex Sans for prose / proportional text rendering.
;; Used by `shr' (eww / mu4e / our markdown-ts preview), mixed-pitch-mode,
;; org-modern title rows, etc.  Modus themes inherit from this for the
;; matching prose faces.
(set-face-attribute 'variable-pitch nil
                    :family "IBM Plex Sans"
                    :height 1.0)

;; Scale `shr-h*' headings so the rendered preview (eww / html email) has
;; a real visual hierarchy.  `modus-themes-heading-N' provides the color
;; and weight; we just set :height on top of that.  Web-ish ladder rather
;; than the subtler markdown-ts source-buffer ladder — a rendered preview
;; is supposed to look like a document, not the source.
(with-eval-after-load 'shr
  (set-face-attribute 'shr-h1 nil :height 2.0   :weight 'bold)
  (set-face-attribute 'shr-h2 nil :height 1.5   :weight 'bold)
  (set-face-attribute 'shr-h3 nil :height 1.25  :weight 'semi-bold)
  (set-face-attribute 'shr-h4 nil :height 1.1   :weight 'semi-bold)
  (set-face-attribute 'shr-h5 nil :height 1.0   :weight 'semi-bold)
  (set-face-attribute 'shr-h6 nil :height 0.9   :weight 'semi-bold)
  ;; Real Unicode bullet for <ul><li>.  Default is the literal string "* "
  ;; which leaves Markdown-source-looking asterisks in the rendered output.
  (setq shr-bullet "• ")
  ;; Table borders — horizontal rule only, no verticals.
  ;;
  ;; shr renders tables by computing per-cell pixel widths from the
  ;; variable-pitch font.  Box-drawing vertical lines (│) are monospace,
  ;; so they don't align with proportional cell content — you end up with
  ;; doubled `││' at row starts, misaligned corners, and a table that
  ;; overflows the window because each cell carries 2ch of border weight.
  ;;
  ;; Keep ─ for horizontal separators (these align fine; they're row-wide
  ;; rules, not column-wise), and use space for verticals + corners.  Net
  ;; result is a GitHub-style "horizontal-rules between rows, no column
  ;; lines" layout that fits the window and reads cleanly.
  (setq shr-table-horizontal-line ?─
        shr-table-vertical-line   ?\s
        shr-table-corner          ?\s))

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
;;
;; Minimal segment set: just enough to know what file, where in it, and
;; the major mode.  The previous workspace segment duplicated the buffer
;; name (tab-bar default `name' is `buffer-name'), eating ~40 chars of
;; modeline real estate showing the same string twice.  Tab-bar already
;; renders workspaces visually at the top of the frame if used.
(use-package mood-line
  :config
  (setq mood-line-glyph-alist
        (append '((:vc-added . ?+))          ; U+1F7A4 not in PragmataPro — use +
                mood-line-glyphs-unicode))

  (setq mood-line-format
        (mood-line-defformat
         :left  (((mood-line-segment-buffer-status) . " ")
                 ((mood-line-segment-buffer-name)   . "  ")
                 ((mood-line-segment-vc)            . " "))
         :right (((mood-line-segment-cursor-position) . "  ")
                 ((mood-line-segment-major-mode)      . " "))))

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
;; Default to the light tinted variant; toggle with `thb/toggle-theme'.
(load-theme 'modus-operandi-tinted t)

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
     go-mode help custom tab-bar))

  ;; eww buffers are read-only by intent (browsing the web / previewing
  ;; markdown).  Default `normal' state allows `i'/`a'/`o' to enter insert
  ;; mode — confusing in a render-only view.  `motion' state has the same
  ;; navigation bindings but no insert entries, so `i' just bells.
  (evil-set-initial-state 'eww-mode 'motion))

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
  (setq which-key-idle-delay 0.25       ; faster than default (1.0)
        which-key-idle-secondary-delay 0.05
        which-key-sort-order 'which-key-key-order-alpha
        which-key-max-description-length 45
        which-key-side-window-location 'bottom
        which-key-side-window-max-height 0.45)
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

;; Cape — extra completion-at-point sources for Corfu.
;; Append these as fallbacks so mode-specific CAPFs like Eglot still win.
(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-file t)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev t)
  (add-to-list 'completion-at-point-functions #'cape-keyword t))


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
  (defface thb-markdown-ts-fence-delim '((t :inherit (shadow fixed-pitch) :extend t))
    "Face for fenced code-block delimiters (the ``` lines).
Shares the code-block background tint so the delimiter lines blend
visually with the block content rather than being a strip of unstyled
text between two filled rows." :group 'thb-markdown-ts)
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

  (defun thb-markdown-ts--double-tilde-strikethrough-p (node)
    "Return non-nil when NODE is an exact ~~strikethrough~~ node.

The markdown-inline grammar also parses single-tilde spans (`~foo~') as
`strikethrough'.  That is too broad for everyday notes because literal
home paths and prose tildes can be mis-highlighted across large regions.
Only render strikethrough for the unambiguous double-tilde form, and do
not treat triple-or-longer tilde runs as a clean match."
    (let ((start (treesit-node-start node))
          (end   (treesit-node-end node)))
      (and (equal (treesit-node-type node) "strikethrough")
           ;; Minimum non-empty exact form: ~~a~~.
           (>= (- end start) 5)
           (not (eq (char-before start) ?~))
           (eq (char-after start) ?~)
           (eq (char-after (1+ start)) ?~)
           (not (eq (char-after (+ start 2)) ?~))
           (eq (char-before end) ?~)
           (eq (char-before (1- end)) ?~)
           (not (eq (char-before (- end 2)) ?~))
           (not (eq (char-after end) ?~)))))

  (defun thb-markdown-ts--fontify-strikethrough (node override start end &rest _)
    "Fontify NODE as strikethrough only for double-tilde spans."
    (when (thb-markdown-ts--double-tilde-strikethrough-p node)
      (treesit-fontify-with-override
       (max (treesit-node-start node) start)
       (min (treesit-node-end node) end)
       'thb-markdown-ts-strikethrough
       override)))

  (defun thb-markdown-ts--child-of-type (node type)
    "Return the first direct child of NODE whose type is TYPE."
    (seq-find (lambda (child) (equal (treesit-node-type child) type))
              (treesit-node-children node)))

  (defun thb-markdown-ts--task-marker-child (node)
    "Return NODE's standard task-list marker child, or nil."
    (or (thb-markdown-ts--child-of-type node "task_list_marker_unchecked")
        (thb-markdown-ts--child-of-type node "task_list_marker_checked")))

  (defun thb-markdown-ts--list-marker-child (node)
    "Return NODE's list marker child, or nil."
    (seq-find (lambda (child)
                (string-prefix-p "list_marker_" (treesit-node-type child)))
              (treesit-node-children node)))

  (defun thb-markdown-ts--fontify-task-list-item (node override start end &rest _)
    "Render a semantic Markdown task-list marker in NODE as a checkbox glyph.

Unlike `prettify-symbols-mode', this only runs for tree-sitter
`task_list_marker_*' nodes, so literal `- [ ]' text inside code blocks stays
literal."
    (when-let* ((list-marker (thb-markdown-ts--list-marker-child node))
                (task-marker (thb-markdown-ts--task-marker-child node))
                (range-start (treesit-node-start list-marker))
                (range-end   (treesit-node-end task-marker))
                (glyph (if (equal (treesit-node-type task-marker)
                                  "task_list_marker_checked")
                           "☑"
                         "☐")))
      (treesit-fontify-with-override
       (max range-start start)
       (min range-end end)
       'thb-markdown-ts-marker
       override)
      (put-text-property range-start range-end
                         'display
                         (propertize glyph 'face 'thb-markdown-ts-marker))))

  ;; Preview-only faces — applied by shr-tag advice (below) inside markdown
  ;; preview eww buffers so we can give code blocks and table headers
  ;; distinct backgrounds without affecting general eww browsing.
  (defface thb-markdown-ts-preview-code-block
    '((t :inherit fixed-pitch :extend t))
    "Background face for <pre> code blocks in the markdown-ts preview."
    :group 'thb-markdown-ts)
  (defface thb-markdown-ts-preview-th
    '((t :weight bold))
    "Background face for <th> cells in markdown-ts preview tables."
    :group 'thb-markdown-ts)

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
       ;; Lists and tasks.  Task-list checkboxes are rendered from semantic
       ;; task_list_marker_* nodes rather than text patterns, so code blocks
       ;; containing literal `- [ ]' remain untouched.
       (list_item (list_marker_minus) @thb-markdown-ts-marker)
       (list_item (list_marker_plus) @thb-markdown-ts-marker)
       (list_item (list_marker_star) @thb-markdown-ts-marker)
       (list_item (list_marker_dot) @thb-markdown-ts-marker)
       (list_item (list_marker_minus) (task_list_marker_unchecked)) @thb-markdown-ts--fontify-task-list-item
       (list_item (list_marker_plus) (task_list_marker_unchecked)) @thb-markdown-ts--fontify-task-list-item
       (list_item (list_marker_star) (task_list_marker_unchecked)) @thb-markdown-ts--fontify-task-list-item
       (list_item (list_marker_dot) (task_list_marker_unchecked)) @thb-markdown-ts--fontify-task-list-item
       (list_item (list_marker_minus) (task_list_marker_checked)) @thb-markdown-ts--fontify-task-list-item
       (list_item (list_marker_plus) (task_list_marker_checked)) @thb-markdown-ts--fontify-task-list-item
       (list_item (list_marker_star) (task_list_marker_checked)) @thb-markdown-ts--fontify-task-list-item
       (list_item (list_marker_dot) (task_list_marker_checked)) @thb-markdown-ts--fontify-task-list-item
       ;; Blockquotes and horizontal rules.
       (block_quote (block_quote_marker) @thb-markdown-ts-marker)
       (block_quote (paragraph) @thb-markdown-ts-blockquote)
       (thematic_break) @thb-markdown-ts-marker)

     :language 'markdown-inline
     :override t
     :feature 'paragraph-inline
     '((emphasis) @thb-markdown-ts-emphasis
       (strong_emphasis) @thb-markdown-ts-strong
       (strikethrough) @thb-markdown-ts--fontify-strikethrough
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
      (let ((bg-code     (modus-themes-get-color-value 'bg-dim))
            (bg-quote    (modus-themes-get-color-value 'bg-blue-nuanced))
            (bg-th       (modus-themes-get-color-value 'bg-inactive)))
        (set-face-attribute 'thb-markdown-ts-code-block         nil :background bg-code)
        (set-face-attribute 'thb-markdown-ts-fence-delim        nil :background bg-code)
        (set-face-attribute 'thb-markdown-ts-blockquote         nil :background bg-quote)
        ;; Preview faces.
        (set-face-attribute 'thb-markdown-ts-preview-code-block nil :background bg-code)
        (set-face-attribute 'thb-markdown-ts-preview-th         nil :background bg-th))))

  (with-eval-after-load 'modus-themes
    (thb/apply-markdown-ts-faces))

  ;; Wrap shr's <pre> and <th> rendering in face overlays so code blocks
  ;; get a visible background (distinct from inline `<code>') and table
  ;; headers get a slightly stronger background than body cells.  Scoped
  ;; to markdown-ts preview buffers via the buffer-local source-file var,
  ;; so plain eww browsing is unaffected.
  (defun thb-markdown-ts-preview--in-preview-buffer-p ()
    (and (boundp 'thb-markdown-ts-preview--source-file)
         thb-markdown-ts-preview--source-file))
  (define-advice shr-tag-pre
      (:around (orig dom) thb-markdown-ts-preview-block-bg)
    "Wrap <pre> output in a code-block face overlay (preview only)."
    (if (thb-markdown-ts-preview--in-preview-buffer-p)
        (let ((start (point)))
          (funcall orig dom)
          (let ((ov (make-overlay start (point))))
            (overlay-put ov 'face 'thb-markdown-ts-preview-code-block)
            (overlay-put ov 'priority -50)))
      (funcall orig dom)))
  (define-advice shr-tag-th
      (:around (orig dom) thb-markdown-ts-preview-th-bg)
    "Wrap <th> output in a header face overlay (preview only)."
    (if (thb-markdown-ts-preview--in-preview-buffer-p)
        (let ((start (point)))
          (funcall orig dom)
          (let ((ov (make-overlay start (point))))
            (overlay-put ov 'face 'thb-markdown-ts-preview-th)
            (overlay-put ov 'priority -50)))
      (funcall orig dom)))

  ;; shr pads table cells with trailing whitespace to align column widths.
  ;; The total row width often slightly exceeds the window because of that
  ;; padding, which shows up as the `$' truncated-line indicator down the
  ;; right edge of the preview — perceived as overflow even when no real
  ;; content is off-screen.  Trim every trailing run of whitespace after
  ;; eww finishes rendering; tables fit the window cleanly.
  (defun thb-markdown-ts-preview--post-render-cleanup ()
    "`eww-after-render-hook' cleanup for markdown preview buffers.

Two passes:

1. Strip trailing whitespace from every line.  shr pads table cells
   with trailing whitespace to align column widths, which exceeds the
   window's char count even when no real content is off-screen.

2. Remove shr's outer table borders — the single-run ─── lines
   immediately above the first row and below the last row.  shr emits
   them in addition to the per-row separators, so the top and bottom of
   every table renders as two parallel rules.  Row separators are
   segmented (────  ────── with internal gaps for column boundaries);
   outer borders are continuous.  We keep the segmented per-row lines."
    (when (thb-markdown-ts-preview--in-preview-buffer-p)
      (let ((inhibit-read-only t))
        ;; Pass 1: trailing whitespace.
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "[ \t]+$" nil t)
            (replace-match "")))
        ;; Pass 2: outer table borders — lines that are only `─' (plus
        ;; outer whitespace) with no internal gap between rules.
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
              (if (and (string-match-p "\\`\\s-*─+\\s-*\\'" line)
                       (not (string-match-p "─[ \t]+─" line)))
                  ;; Continuous-rule line: delete it (and its newline).
                  (delete-region (line-beginning-position)
                                 (min (point-max) (1+ (line-end-position))))
                (forward-line 1))))))))
  (add-hook 'eww-after-render-hook #'thb-markdown-ts-preview--post-render-cleanup)

  ;; --- Phase 3: semantic checkbox rendering -----------------------------
  ;;
  ;; Task-list checkboxes are rendered in `thb-markdown-ts--rules' from
  ;; tree-sitter's `task_list_marker_unchecked' / `task_list_marker_checked'
  ;; nodes.  Avoid `prettify-symbols-mode' here: it is text-pattern based and
  ;; will happily prettify literal `- [ ]' inside fenced code blocks.

  ;; --- Phase 3b: heading folding via outline-minor-mode ------------------
  ;;
  ;; Evil's z* fold commands know how to talk to `outline-minor-mode'.  Enable
  ;; it for markdown-ts buffers and teach outline how to find ATX headings.
  ;; The search function validates regex candidates against the tree-sitter
  ;; parse so a literal "# heading" inside a fenced code block is not treated
  ;; as a foldable document heading.

  (defconst thb-markdown-ts-outline-regexp
    "^[ \t]*\\(#\\{1,6\\}\\)\\(?:[ \t]+\\|$\\)"
    "Regexp matching ATX Markdown headings for `outline-minor-mode'.")

  (defun thb-markdown-ts-outline-level ()
    "Return outline level for the current ATX Markdown heading."
    (length (match-string 1)))

  (defun thb-markdown-ts--atx-heading-match-p ()
    "Return non-nil when the current outline regexp match is an ATX heading."
    (when-let* ((pos (match-beginning 1))
                (node (treesit-node-at pos 'markdown))
                (heading (treesit-parent-until
                          node
                          (lambda (n) (equal (treesit-node-type n) "atx_heading"))
                          t)))
      (and (<= (treesit-node-start heading) pos)
           (< pos (treesit-node-end heading)))))

  (defun thb-markdown-ts--outline-search (&optional bound move backward looking-at)
    "Search for a semantic Markdown outline heading.

Implements `outline-search-function' by trying `outline-regexp' matches and
keeping only those backed by a tree-sitter `atx_heading' node.  When MOVE is
non-nil and no heading is found, move to BOUND or the buffer edge, matching
`re-search-forward' / `re-search-backward' with a non-nil noerror move arg."
    (cond
     (looking-at
      (and (looking-at outline-regexp)
           (thb-markdown-ts--atx-heading-match-p)))
     (backward
      (catch 'found
        (while (re-search-backward outline-regexp bound t)
          (when (thb-markdown-ts--atx-heading-match-p)
            (throw 'found t)))
        (when move (goto-char (or bound (point-min))))
        nil))
     (t
      (catch 'found
        (while (re-search-forward outline-regexp bound t)
          (when (thb-markdown-ts--atx-heading-match-p)
            (throw 'found t)))
        (when move (goto-char (or bound (point-max))))
        nil))))

  (defun thb-markdown-ts-setup-outline ()
    "Enable outline folding for `markdown-ts-mode'."
    (setq-local outline-regexp thb-markdown-ts-outline-regexp)
    (setq-local outline-level #'thb-markdown-ts-outline-level)
    (setq-local outline-search-function #'thb-markdown-ts--outline-search)
    (outline-minor-mode 1))

  (with-eval-after-load 'evil
    (evil-define-key 'normal markdown-ts-mode-map (kbd "TAB") #'evil-toggle-fold)
    (evil-define-key 'normal markdown-ts-mode-map (kbd "<tab>") #'evil-toggle-fold))

  (defun thb-markdown-ts--range-settings ()
    "Return range settings that keep markdown-inline inside inline nodes."
    (treesit-range-rules
     :embed 'markdown-inline
     :host 'markdown
     '((inline) @capture)))

  ;; `markdown-ts-mode' calls `treesit-major-mode-setup' before mode hooks.
  ;; Range settings must therefore be present before `markdown-ts-setup' runs;
  ;; setting them only in `markdown-ts-mode-hook' leaves the inline parser over
  ;; the whole buffer, where tildes/backticks from unrelated paragraphs can be
  ;; misparsed as giant inline spans.
  (setq markdown-ts--treesit-settings (thb-markdown-ts--rules))
  (defun thb-markdown-ts--pre-setup ()
    "Install parser range settings before `treesit-major-mode-setup'."
    (setq-local treesit-range-settings (thb-markdown-ts--range-settings)))
  (advice-add 'markdown-ts-setup :before #'thb-markdown-ts--pre-setup)

  (defun thb/markdown-setup ()
    "Per-buffer markdown setup: visual line wrapping and source prettification.

Custom font-lock rules and parser ranges are installed before
`treesit-major-mode-setup' via `markdown-ts--treesit-settings' and
`thb-markdown-ts--pre-setup'.  This hook handles display-only toggles that
can safely run after the major mode is initialized."
    (visual-line-mode 1)
    (thb-markdown-ts-setup-outline)
    ;; `display' is used by the semantic checkbox renderer.  Tell font-lock to
    ;; clear it before refontifying changed regions, otherwise stale checkbox
    ;; glyphs can survive edits that turn a task item into plain text.
    (add-to-list 'font-lock-extra-managed-props 'display)
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

  ;; --- Phase 5: eww preview with auto-refresh ---------------------------
  ;;
  ;; `SPC m v p' toggles a rendered HTML view of the current markdown file
  ;; in an `eww' buffer (same window, not split).  Calling the toggle from
  ;; inside the preview returns to the source; `q' also works there via a
  ;; local rebinding (evil's normal-state `q' otherwise records a macro).
  ;;
  ;; The renderer is `md2html --github' (md4c), ~300x faster than pandoc
  ;; on a 33KB document, with GFM tables, task lists, strikethrough, and
  ;; fenced code blocks with language classes.  Output HTML is written to
  ;; a per-session temp directory and wrapped with `<base href>' pointing
  ;; at the source's directory so relative <img> / anchor links resolve.
  ;;
  ;; The preview auto-refreshes via `file-notify-add-watch' on the source
  ;; file path so agent rewrites land in the preview essentially instantly.
  ;; The callback is wrapped in `condition-case' to keep the watch alive
  ;; across rendering errors, and — critically for atomic-rename rewrites
  ;; (the common agent pattern: write `.tmp` then rename onto target) —
  ;; the watch re-establishes itself on `stopped' / `deleted' / `renamed'
  ;; events, because macOS kqueue watches the inode, not the path, and the
  ;; original inode is gone after a rename.

  (defcustom thb-markdown-ts-preview-renderer
    '("md2html" "--github")
    "Command + args to convert markdown to HTML on stdout.
The source file path is appended as the final argument.
Swap for any fast md→html binary (e.g. cmark-gfm) if you prefer."
    :type '(repeat string)
    :group 'thb-markdown-ts)

  (defvar thb-markdown-ts-preview--temp-dir nil
    "Per-Emacs-session temp directory for rendered preview HTML.")

  (defvar-local thb-markdown-ts-preview--source-file nil
    "Source markdown file the current preview buffer is rendering.")
  (defvar-local thb-markdown-ts-preview--html-file nil
    "Generated HTML file backing the current preview buffer.")
  (defvar-local thb-markdown-ts-preview--watch nil
    "`file-notify' descriptor watching the source file.
Replaced when the watch dies on rename/delete (see -on-change).")

  (defun thb-markdown-ts-preview--ensure-temp-dir ()
    (or (and thb-markdown-ts-preview--temp-dir
             (file-directory-p thb-markdown-ts-preview--temp-dir)
             thb-markdown-ts-preview--temp-dir)
        (setq thb-markdown-ts-preview--temp-dir
              (make-temp-file "thb-md-preview-" t))))

  (defun thb-markdown-ts-preview--html-path-for (source-file)
    "Return a stable per-source HTML path under the session temp dir."
    (expand-file-name
     (format "%s-%s.html"
             (file-name-base source-file)
             (substring (secure-hash 'sha1 (expand-file-name source-file)) 0 8))
     (thb-markdown-ts-preview--ensure-temp-dir)))

  (defun thb-markdown-ts-preview--buffer-name (source-file)
    (format "*md preview: %s*" (file-name-nondirectory source-file)))

  (defun thb-markdown-ts-preview--write-html (source-file html-file)
    "Render SOURCE-FILE → HTML-FILE via `thb-markdown-ts-preview-renderer'.
Wraps the renderer's output with a minimal document and `<base href>'
so relative paths in the source resolve against its directory."
    (let* ((cmd  (car thb-markdown-ts-preview-renderer))
           (args (append (cdr thb-markdown-ts-preview-renderer)
                         (list source-file)))
           (src-dir (file-name-directory (expand-file-name source-file))))
      (with-temp-buffer
        (let ((status (apply #'call-process cmd nil (current-buffer) nil args)))
          (unless (zerop status)
            (error "%s exited %d: %s" cmd status
                   (buffer-substring-no-properties (point-min) (point-max)))))
        (goto-char (point-min))
        (insert "<!DOCTYPE html><html><head>"
                (format "<base href=\"file://%s\">" src-dir)
                "<meta charset=\"utf-8\"></head><body>\n")
        (goto-char (point-max))
        (insert "\n</body></html>\n")
        (let ((coding-system-for-write 'utf-8))
          (write-region (point-min) (point-max) html-file nil 'no-message)))))

  (defun thb-markdown-ts-preview--apply-display-tweaks ()
    "Apply preview-buffer display tweaks: hide eww chrome, kill fringes
and line numbers, let shr fill the full window width.  Idempotent and
safe to call repeatedly — we call it on every preview activation, both
fresh and refresh, so changes survive across `eww-reload' and re-toggles
that would otherwise reset buffer-local state."
    ;; Hide eww's URL/title header line.
    (setq-local eww-header-line-format nil)
    (setq header-line-format nil)
    ;; Strip fringes (and update any window already showing this buffer).
    (setq-local left-fringe-width 0)
    (setq-local right-fringe-width 0)
    (when-let ((win (get-buffer-window (current-buffer))))
      (set-window-fringes win 0 0))
    ;; Let shr fill the window edge-to-edge.
    (setq-local shr-width nil)
    (setq-local shr-max-width nil)
    (setq-local shr-indentation 0)
    ;; Line numbers off (even though global-display-line-numbers-mode is on).
    (display-line-numbers-mode -1)
    ;; Wrap visually instead of truncating with `$'.  shr uses pixel-based
    ;; line widths against the variable-pitch font, but Emacs' truncation
    ;; indicator uses character columns against the monospace baseline.
    ;; The two can disagree by a few chars for proportional text — lines
    ;; visually fit the window but the buffer has more chars than columns,
    ;; so `$' flags non-existent overflow.  Wrapping avoids the artifact.
    (setq-local truncate-lines nil)
    (setq-local word-wrap t))

  (defun thb-markdown-ts-preview--refresh ()
    "Re-render the source file into the HTML, then eww-reload the buffer.
Must be called inside a live preview buffer."
    (let ((src  thb-markdown-ts-preview--source-file)
          (html thb-markdown-ts-preview--html-file))
      (when (and src html (file-readable-p src))
        (thb-markdown-ts-preview--write-html src html)
        (ignore-errors (eww-reload)))))

  (defun thb-markdown-ts-preview--watch-source (preview-buffer)
    "Install (or re-install) a `file-notify' watch on PREVIEW-BUFFER's source.
Returns the descriptor.  The callback dispatches on action:

  - `changed' / `attribute-changed' / `created' — re-render the preview.
  - `stopped' / `deleted' / `renamed' — the kqueue watch is dead because
    the watched inode is gone (typical with atomic rewrite: write `.tmp`
    then rename onto target).  Schedule a re-watch on the path so we
    keep seeing future changes, and re-render now to reflect the rename.

The callback is wrapped in `condition-case' so rendering errors don't
propagate and don't risk auto-removal of the watch."
    (let ((src (buffer-local-value 'thb-markdown-ts-preview--source-file
                                   preview-buffer)))
      (file-notify-add-watch
       src '(change attribute-change)
       (lambda (event)
         (condition-case err
             (pcase-let ((`(,_descriptor ,action . ,_rest) event))
               (when (buffer-live-p preview-buffer)
                 (with-current-buffer preview-buffer
                   (cond
                    ;; Inode is gone (rename/delete).  Re-establish watch
                    ;; after a short delay so the new file is in place.
                    ((memq action '(stopped deleted renamed))
                     (run-at-time
                      0.05 nil
                      (lambda ()
                        (when (buffer-live-p preview-buffer)
                          (with-current-buffer preview-buffer
                            (when (file-readable-p
                                   thb-markdown-ts-preview--source-file)
                              (setq-local thb-markdown-ts-preview--watch
                                          (thb-markdown-ts-preview--watch-source
                                           preview-buffer))
                              (thb-markdown-ts-preview--refresh)))))))
                    ;; Normal in-place edit / attribute change.
                    ((memq action '(changed attribute-changed created))
                     (thb-markdown-ts-preview--refresh))))))
           (error (message "markdown-ts preview watch error: %s" err)))))))

  (defun thb-markdown-ts-preview--cleanup ()
    "Buffer-killed hook: remove file-notify watch + scrub the HTML file."
    (when thb-markdown-ts-preview--watch
      (ignore-errors (file-notify-rm-watch thb-markdown-ts-preview--watch))
      (setq thb-markdown-ts-preview--watch nil))
    (when (and thb-markdown-ts-preview--html-file
               (file-exists-p thb-markdown-ts-preview--html-file))
      (ignore-errors (delete-file thb-markdown-ts-preview--html-file))))

  (defun thb-markdown-ts-preview ()
    "Toggle a live-rendered HTML preview of the current markdown buffer.

In `markdown-ts-mode' → swap the current window to an `eww' preview of
the source file.  In a markdown-ts-mode preview buffer → quit back to the
source.  The preview auto-refreshes whenever the source file changes on
disk (e.g. when an agent rewrites it).  `q' in the preview also quits."
    (interactive)
    (cond
     ;; Already inside a preview buffer: quit back.
     ((and (eq major-mode 'eww-mode)
           (local-variable-p 'thb-markdown-ts-preview--source-file)
           thb-markdown-ts-preview--source-file)
      (quit-window))
     ;; In markdown source: render + show.
     ((eq major-mode 'markdown-ts-mode)
      (let ((src (buffer-file-name)))
        (unless src (user-error "Buffer is not visiting a file"))
        (when (buffer-modified-p) (save-buffer))
        (let* ((html-file    (thb-markdown-ts-preview--html-path-for src))
               (preview-name (thb-markdown-ts-preview--buffer-name src))
               (existing     (get-buffer preview-name)))
          (thb-markdown-ts-preview--write-html src html-file)
          (cond
           ;; Refresh existing preview and re-show in current window.
           ((buffer-live-p existing)
            (switch-to-buffer existing)
            (thb-markdown-ts-preview--apply-display-tweaks)
            (ignore-errors (eww-reload)))
           (t
            (eww-open-file html-file)
            ;; eww just made the new buffer current; rename + wire state.
            (rename-buffer preview-name)
            (thb-markdown-ts-preview--apply-display-tweaks)
            (setq-local thb-markdown-ts-preview--source-file src)
            (setq-local thb-markdown-ts-preview--html-file html-file)
            (setq-local thb-markdown-ts-preview--watch
                        (thb-markdown-ts-preview--watch-source (current-buffer)))
            (add-hook 'kill-buffer-hook #'thb-markdown-ts-preview--cleanup nil t)
            ;; Local `q' override: evil's normal-state `q' is
            ;; `evil-record-macro' which shadows eww's quit binding.
            (when (boundp 'evil-normal-state-local-map)
              (setq-local evil-normal-state-local-map
                          (let ((map (make-sparse-keymap)))
                            (set-keymap-parent map (or evil-normal-state-local-map
                                                       (make-sparse-keymap)))
                            (define-key map (kbd "q") #'thb-markdown-ts-preview)
                            map))))))))
     (t (user-error "Not in markdown-ts-mode or its preview"))))

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
    "mst" '(evil-toggle-fold                  :which-key "toggle fold")
    "msj" '(thb-markdown-ts-move-subtree-down :which-key "move down")
    "msk" '(thb-markdown-ts-move-subtree-up   :which-key "move up")
    "msn" '(thb-markdown-ts-narrow-to-subtree :which-key "narrow")
    "msN" '(widen                              :which-key "widen")
    "msd" '(thb-markdown-ts-cut-subtree        :which-key "cut")

    ;; --- SPC m l: links ---
    "ml"  '(:ignore t :which-key "links")
    "mlo" '(thb-markdown-ts-open-link   :which-key "open at point")
    "mll" '(thb-markdown-ts-insert-link :which-key "insert link")

    ;; --- SPC m v: preview ---
    "mv"  '(:ignore t :which-key "preview")
    "mvp" '(thb-markdown-ts-preview     :which-key "eww (shr render)")
    "mvr" '(thb-md-render-toggle        :which-key "diy render (tree-sitter)")))

;; DIY markdown renderer — bypasses HTML entirely and walks the
;; tree-sitter-markdown AST to emit fontified text directly.  v0.1; sits
;; alongside the shr-based `SPC m v p' preview for comparison.
(use-package thb-markdown-render
  :ensure nil
  :commands (thb-md-render-file thb-md-render-toggle))

;; In-place decoration for markdown-ts-mode source buffers — hides
;; delimiters (* ` ~ etc.) and link URLs so the source reads like the
;; rendered output.  Reveals decorated regions on cursor-entry, like
;; org-appear does for org-mode.  Auto-enables on markdown-ts-mode.
(use-package thb-markdown-decor
  :ensure nil
  :hook (markdown-ts-mode . thb-markdown-decor-mode))


;;; ============================================================
;;; Beads (Issue Tracking)
;;; ============================================================

(use-package hierarchy)

(use-package beads
  :vc (:url "https://github.com/ChristianTietze/beads.el" :lisp-dir "lisp" :rev :newest)
  :commands (beads beads-project-list beads-create-issue beads-activity beads-stats)
  :config
  ;; Route every beads.el call through `bd --global' so M-x beads opens
  ;; the tb-workflow shared-server dolt database (beads_global) — the
  ;; same workspace the pi agent's tbwf_* tools read/write. Three pieces
  ;; of plumbing:
  ;;
  ;;   1. CLI selection. Switch from `br' to `bd' since only `bd --global'
  ;;      reaches the shared-server dolt DB; `br' uses its own SQLite at
  ;;      ~/.beads/beads.db. Force `cli' connection strategy because bd 1.x
  ;;      dropped the `daemon' subcommand — the previous br pin existed
  ;;      only because auto-detect kept trying to start that missing daemon.
  ;;
  ;;   2. Env. BEADS_DOLT_SHARED_SERVER=1 unlocks --global in bd. BEADS_DIR
  ;;      points at tb-workflow's dedicated host workspace (mirrors
  ;;      defaultGlobalHostDir in extensions/tb-workflow/src/config.ts) to
  ;;      work around the bd 1.0.4 PROJECT IDENTITY MISMATCH bug — see
  ;;      gastownhall/beads#3476. setenv leaks into Emacs child processes,
  ;;      which is the intent: any bd call from M-x shell or org-babel
  ;;      should also land in the global workspace.
  ;;
  ;;   3. Arg injection. Prepend --global on every bd invocation via
  ;;      :filter-return advice on the bd backend's :cli-extra-flags slot.
  ;;
  ;;   4. Stderr suppression. bd emits warnings ("beads.role not configured"
  ;;      when cwd's git config lacks the key, "Showing N issues..." on
  ;;      truncated lists) on stderr. beads.el's `call-process' uses
  ;;      destination=`t' which mixes stderr into the same buffer as
  ;;      stdout, so `json-read' from point-min trips on the leading
  ;;      stderr text and reports a parse error (the whole buffer,
  ;;      including the valid JSON tail, ends up in *Messages*).
  ;;      An :around advice on `beads-backend-cli-execute' rebinds
  ;;      `call-process' for the dynamic extent of that one call so the
  ;;      destination becomes `(t nil)' (stdout to buffer, stderr
  ;;      discarded). For --json calls (which beads.el always issues),
  ;;      genuine errors arrive on stdout as `{"error": ...}', so
  ;;      discarding stderr doesn't hide real failure modes.
  ;;
  ;; The previous autoupdate-skip-when-no-.beads guard is dropped: with
  ;; `bd --global' the database is always reachable, so cli-fallback's
  ;; nil-project-root branch is harmless and refresh just works.
  (setenv "BEADS_DOLT_SHARED_SERVER" "1")
  (setenv "BEADS_DIR"
          (expand-file-name
           ".beads"
           (format "/var/tmp/tb-workflow-%s/bd-global-host"
                   (or (getenv "USER") (user-login-name)))))

  (setq beads-cli-program "bd"
        beads-client-connection-strategy 'cli
        beads-autoupdate-enable t
        beads-list-highlight-p0-rows t
        beads-detail-render-markdown t
        beads-verbose t)

  (with-eval-after-load 'beads-backend-bd
    (define-advice beads-backend-bd--cli-extra-flags
        (:filter-return (extra) thb/prepend-global)
      (cons "--global" extra)))

  (with-eval-after-load 'beads-backend
    (require 'cl-lib)
    (define-advice beads-backend-cli-execute
        (:around (orig &rest args) thb/discard-bd-stderr)
      (cl-letf* ((real-call-process (symbol-function 'call-process))
                 ((symbol-function 'call-process)
                  (lambda (program &optional infile destination display &rest argv)
                    (apply real-call-process program infile
                           (if (eq destination t) (list t nil) destination)
                           display argv))))
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
