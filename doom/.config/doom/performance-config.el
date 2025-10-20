;;; performance-config.el -*- lexical-binding: t; -*-

;;
;; Performance Optimizations
;;

;; Modeline performance optimizations (compatible with Doom defaults)
(after! doom-modeline
  (setq doom-modeline-vcs-max-length 15           ; Shorter git branch names
        doom-modeline-enable-word-count nil       ; Disable word count
        doom-modeline-indent-info nil             ; Hide indentation info
        doom-modeline-env-version nil             ; Hide environment versions (python, go, etc.)
        doom-modeline-checker-simple-format t     ; Simpler syntax checker display
        doom-modeline-workspace-name nil          ; Hide workspace name
        doom-modeline-display-default-persp-name nil ; Don't show default perspective
        doom-modeline-total-line-number nil       ; Hide total line count (keep current line)
        doom-modeline-height 25))                 ; Reduce modeline height

;; Enhanced Magit performance (building on Doom's optimizations)
(after! magit
  ;; Core performance settings (compatible with Doom defaults)
  (setq magit-revision-show-gravatars nil       ; Disable gravatars (network requests)
        magit-diff-refine-hunk nil             ; Don't auto-refine hunks
        magit-section-cache-visibility t        ; Cache section visibility
        magit-log-section-commit-count 10       ; Fewer commits in log sections
        magit-section-initial-visibility-alist ; Hide expensive sections by default
        '((stashes . hide)
          (untracked . hide)
          (unpushed . hide)
          (unpulled . hide))
        ;; Reduce status sections for better performance
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
          magit-insert-staged-changes))

  ;; Process I/O optimization - increase from Doom's 64KB default
  (setq read-process-output-max (* 4 1024 1024)  ; 4MB for faster git operations
        ;; GC tuning specifically for Magit operations
        magit-refresh-verbose nil                 ; Reduce refresh verbosity
        magit-diff-highlight-trailing nil)       ; Disable trailing whitespace highlighting

  ;; Remove expensive status sections for better performance
  (remove-hook 'magit-status-sections-hook #'magit-insert-untracked-files)
  (remove-hook 'magit-status-sections-hook #'magit-insert-stashes)

  ;; Repository scanning limits
  (when (boundp 'magit-repository-directories)
    (setq magit-repository-directories '(("~/Documents" . 0) ; No scanning of docs
                                         ("~/.config" . 1))))) ; Shallow config scan

;; General Emacs performance optimizations (beyond Doom's defaults)
(setq ;; Optimize completion systems
 company-idle-delay 0.2                     ; Slight delay before company completion
 ;; File handling optimizations
 auto-save-default nil                      ; Disable auto-save files (Doom may override)
 make-backup-files nil                      ; Disable backup files
 create-lockfiles nil                       ; Disable lock files
 ;; Display optimizations
 frame-inhibit-implied-resize t             ; Don't resize frame implicitly
 ;; Reduce minibuffer noise
 echo-keystrokes 0.1                        ; Show keystrokes faster
 eldoc-idle-delay 0.1                       ; Faster eldoc (Doom default is higher)
 which-key-idle-delay 0.4                   ; Faster which-key (Doom default is 1.0)
 ;; Compilation optimizations
 compilation-scroll-output 'first-error    ; Only scroll to first error
 compilation-skip-threshold 2)             ; Skip warnings in compilation

;; Enhanced LSP performance (complementing existing config)
(after! lsp-mode
  (setq lsp-idle-delay 0.5                      ; Increase delay before LSP actions
        lsp-completion-provider :none           ; Use company/corfu instead of LSP
        lsp-eldoc-enable-hover nil              ; Disable hover eldoc (use lsp-ui if needed)
        lsp-signature-auto-activate nil         ; Disable automatic signature help
        lsp-lens-enable nil                     ; Disable code lens globally
        lsp-symbol-highlighting-skip-current t  ; Don't highlight current symbol
        lsp-enable-folding nil                  ; Disable LSP folding (use Doom's)
        lsp-enable-snippet nil                  ; Disable LSP snippets (use yasnippet)
        lsp-enable-symbol-highlighting nil      ; Disable symbol highlighting
        lsp-enable-links nil))                  ; Disable clickable links

;; Corfu/completion performance (if using corfu)
(after! corfu
  (setq corfu-auto-delay 0.2                    ; Slight delay before completion
        corfu-auto-prefix 3                     ; Require 3 chars before completion
        corfu-cycle t))                         ; Enable cycling through candidates

;; Buffer and window management optimizations
(setq split-width-threshold 160                 ; Prefer horizontal splits on wide screens
      split-height-threshold 80                 ; Control vertical splits
      window-combination-resize t               ; Resize windows proportionally
      ;; Projectile performance (complementing existing config)
      projectile-sort-order 'recently-active)   ; Smart project sorting

;; Large file handling - disable expensive modes automatically
(defun thb/disable-expensive-modes-for-large-files ()
  "Disable expensive modes for files larger than 1MB."
  (when (and (buffer-file-name)
             (> (buffer-size) (* 1024 1024)))    ; 1MB threshold
    (fundamental-mode)                           ; Switch to fundamental mode
    (font-lock-mode -1)                         ; Disable syntax highlighting
    (when (and (boundp 'lsp-mode) lsp-mode)
      (lsp-mode -1))                            ; Disable LSP
    (when (and (boundp 'flycheck-mode) flycheck-mode)
      (flycheck-mode -1))                       ; Disable flycheck
    (when (and (boundp 'company-mode) company-mode)
      (company-mode -1))                        ; Disable company
    (message "Large file detected - disabled expensive modes for performance")))

(add-hook 'find-file-hook #'thb/disable-expensive-modes-for-large-files)

(provide 'performance-config)