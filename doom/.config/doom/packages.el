;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! catppuccin-theme)
(package! git-auto-commit-mode)

;; Personal org-mode packages
(package! consult-org-roam)
(package! org-super-agenda)
(package! org-tidy)

;; Load local packages (not synced to repo)
(load! "packages-local" nil t)
