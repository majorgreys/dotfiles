;;; init.el -*- lexical-binding: t; -*-

(def-package! olivetti
  :config
  (map! (:map olivetti-mode-map
          "M-[" #'olivetti-shrink
          "M-]" #'olivetti-expand)))

(def-package! centered-cursor-mode)

(def-package! monotropic-theme)
(def-package! eink-theme)
(def-package! writeroom-mode
  :config
  (map! (:map writeroom-mode-map
          "M-[" #'writeroom-increase-width
          "M-]" #'writeroom-decrease-width)))

(def-package! wttrin
  :init
  (setq wttrin-default-cities '("Brooklyn_NY"))
  (setq wttrin-default-accept-language '("Accept-Language" . "en-US")))

(doom! :app
       email
       (write
        +wordnut
        +langtool))


(defun reload-fonts (frame)
  (set-face-attribute 'variable-pitch frame :font doom-variable-pitch-font))
(add-hook 'after-make-frame-functions 'reload-fonts)

