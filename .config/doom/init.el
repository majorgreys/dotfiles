;;; init.el -*- lexical-binding: t; -*-

(setq doom-font (font-spec :family "Source Code Pro" :size 16))
(setq doom-big-font (font-spec :family "Source Code Pro" :size 20))

(def-package! olivetti
  :config
  (map! (:map olivetti-mode-map
          "M-[" #'olivetti-shrink
          "M-]" #'olivetti-expand)))

;(def-package-hook! pandoc-mode)

(def-package! centered-cursor-mode)

(def-package-hook! pdf-tools :disable)

(defun reload-fonts (frame)
  (set-face-attribute 'variable-pitch frame :font doom-variable-pitch-font))
(add-hook 'after-make-frame-functions 'reload-fonts)

