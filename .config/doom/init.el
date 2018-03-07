;;; init.el -*- lexical-binding: t; -*-

(def-package! nord-theme)

(def-package! zotxt
  :config
  (setq zotxt-default-bibliography-style "chicago-note-bibliography"))

(doom! :app
       ; email
       (write
        +wordnut
        +langtool))
