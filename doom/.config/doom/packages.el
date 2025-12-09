;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! catppuccin-theme)
(package! git-auto-commit-mode)

;; Personal org-mode packages
(package! consult-org-roam)
(package! org-super-agenda)
(package! org-tidy)
(package! ob-mermaid)
(package! sync-docs :recipe (:type git :host github :repo "laertida/sync-docs.el"))

(package! plz)
;; Semantic search and indexing
(package! org-db-v3 :recipe (:type git :host github :repo "jkitchin/org-db-v3" :files ("elisp/*.el" "python/*")))

;; Load local packages (not synced to repo)
(load! "packages-local" nil t)
