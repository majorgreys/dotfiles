;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! modus-themes)

;; Personal org-mode packages
(package! consult-org-roam)
(package! vulpea)
(package! vulpea-ui)
(package! vulpea-journal)
(package! consult-vulpea)
(package! org-tidy)
(package! org-confluence-publish :recipe (:type git :repo "git@github.com:majorgreys/org-confluence-publish.git"))
(package! ob-mermaid)
(package! ob-d2)

(package! shell-maker)
(package! acp)
(package! agent-shell)
(package! agent-shell-attention)
(package! agent-shell-manager)

;; Load local packages (not synced to repo)
(load! "packages-local" nil t)
