;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! catppuccin-theme)
(package! git-auto-commit-mode)

;; Personal org-mode packages
(package! consult-org-roam)
(package! org-super-agenda)
(package! org-tidy)
(package! org-confluence-publish :recipe (:type git :repo "git@github.com:majorgreys/org-confluence-publish.git"))
(package! ob-mermaid)

(package! plz)
;; Semantic search and indexing
(package! org-db-v3 :recipe (:type git :host github :repo "jkitchin/org-db-v3" :files ("elisp/*.el" "python/*")))

;; Load local packages (not synced to repo)
(load! "packages-local" nil t)
