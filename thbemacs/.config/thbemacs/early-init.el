;;; early-init.el --- Pre-initialization -*- lexical-binding: t; -*-

;; This file runs before init.el and before the package system or GUI
;; initializes. Use it for settings that must be applied extremely early.
;; Inspired by what Doom Emacs does under the hood.

;;; GC Tuning
;; Raise GC threshold during init to avoid collection pauses.
;; 64MB is generous — we restore to 8MB after init completes.
(setq gc-cons-threshold (* 64 1024 1024)
      gc-cons-percentage 0.6)

;;; File Name Handler Optimization
;; `file-name-handler-alist` is consulted on every file load. Disabling it
;; during init speeds up `require` and `load` significantly.
(defvar thb/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;;; Restore Defaults After Init
;; Use `emacs-startup-hook` (runs after init.el and all startup files).
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 8 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist thb/file-name-handler-alist)
            (makunbound 'thb/file-name-handler-alist)))

;;; Package System
;; We initialize packages manually in init.el, so disable the automatic
;; package-initialize that would otherwise run between early-init and init.
(setq package-enable-at-startup nil)

;;; Native Compilation (Emacs 29+)
;; Suppress noisy native-comp warnings. Enable JIT compilation.
(when (featurep 'native-compile)
  (setq native-comp-async-report-warnings-errors 'silent
        native-comp-jit-compilation t
        native-comp-deferred-compilation t))   ; alias used by some versions

;;; GUI Chrome — Suppress Before First Frame
;; Turning these off here is faster than toggling them after the frame draws.
(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (internal-border-width . 16)))

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;;; macOS Frame Appearance
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  (add-to-list 'default-frame-alist '(width . 180))
  (add-to-list 'default-frame-alist '(height . 55)))

;;; Skip Site Init
;; Avoid loading site-wide startup files (e.g., /etc/emacs/site-start.el).
(setq site-run-file nil)

;;; early-init.el ends here
