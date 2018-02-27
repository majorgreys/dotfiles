;;; init.el -*- lexical-binding: t; -*-

(def-package! nord-theme)

(def-package! zotxt
  :config
  (setq org-zotxt-default-search-method :everything)
  (setq zotxt-default-bibliography-style "chicago-note-bibliography"))

;(def-package! org-zotxt
;  :init
;  (add-hook 'org-mode-hook #'org-zotxt-mode)
;  :config
;  (setq org-zotxt-link-description-style :betterbibtexkey))

(def-package! wttrin
  :init
  (setq wttrin-default-cities '("Brooklyn_NY"))
  (setq wttrin-default-accept-language '("Accept-Language" . "en-US")))

(doom! :app
       email
       (write
        +wordnut
        +langtool))
