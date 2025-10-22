;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! catppuccin-theme)
(package! claude-code-ide :recipe (:type git :host github :repo "manzaltu/claude-code-ide.el"))
(package! git-auto-commit-mode)

;; Personal org-mode packages
(package! consult-org-roam)
(package! org-super-agenda)
(package! org-tidy)
(package! org-roam-skill :recipe (:type git :host github :repo "majorgreys/org-roam-skill"))

;; Load local packages (not synced to repo)
(load! "packages-local" nil t)
