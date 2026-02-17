;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! modus-themes)
(package! nano-theme :recipe (:type git :host github :repo "rougier/nano-theme"))

;; Personal org-mode packages
(package! consult-org-roam)
(package! org-super-agenda)
(package! org-tidy)
(package! org-confluence-publish :recipe (:type git :repo "git@github.com:majorgreys/org-confluence-publish.git"))
(package! ob-mermaid)
(package! ob-d2)

(package! shell-maker)
(package! acp)
(package! agent-shell)

;; Semantic search and indexing (plz is a dependency of org-db-v3)
(package! plz)
(package! org-db-v3 :recipe (:type git :host github :repo "jkitchin/org-db-v3" :files ("elisp/*.el" "python/*")))

;; Load local packages (not synced to repo)
(load! "packages-local" nil t)
