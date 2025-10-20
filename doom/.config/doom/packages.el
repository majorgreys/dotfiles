;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! catppuccin-theme)
(package! claude-code-ide :recipe (:type git :host github :repo "manzaltu/claude-code-ide.el"))
(package! git-auto-commit-mode)

;; Load local packages (not synced to repo)
(load! "packages-local" nil t)
