(package-initialize)

(require 'package)
(setq package-enable-at-startup nil)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

;; Don't litter my init file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; add use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Essential settings
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(setq-default left-fringe-width nil)
(setq-default indicate-empty-lines t)
(setq-default indent-tabs-mode nil)

(setq visible-bell t)
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)
(setq custom-safe-themes t)
(column-number-mode t)
(setq tab-width 4)
(setq tramp-default-method "ssh")

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)

(use-package nord-theme
  :ensure t
  :config
  (load-theme 'nord t))

(use-package which-key
  :ensure t)

(use-package evil 
  :ensure t
  :config
  (evil-mode 1)
  )

(use-package helm
  :ensure t
  :diminish helm-mode
  :commands helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (define-key helm-map (kbd "S-SPC") 'helm-toggle-visible-mark)
  (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme))))

(use-package writeroom-mode
  :ensure t)

(use-package focus
  :ensure t)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package pandoc-mode
  :ensure t
  :config
  (add-hook 'markdown-mode-hook 'pandoc-mode)
  ;; (add-hook 'org-mode-hook 'pandoc-mode)
  (add-hook 'pandoc-mode-hook 'pandoc-load-default-settings))

(use-package ess
  :ensure t
  :config
  (setq ess-nuke-trailing-whitespace-p t)
  (setq ess-default-style 'RStudio)
  (setq ess-eval-visibly 'nowait) ; don't hog Emacs
  (setq ess-ask-for-ess-directory nil) ; don't ask for dir when starting a process
  (setq ess-eldoc-show-on-symbol t) ; show eldoc on symbol instead of only inside of parens
  (setq ess-use-ido nil) ; rely on helm instead of ido
  (progn
    ;; Save R history in one place rather than making .Rhistory files
    ;; everywhere. Make that folder if needed.
    (setq ess-history-directory (concat user-emacs-directory "var/Rhist/"))
    (mkdir ess-history-directory t))
  (setq ess-pdf-viewer-pref "emacsclient"))

(add-hook 'window-setup-hook
          (lambda ()
            (when (memq window-system '(x))
              (add-to-list 'default-frame-alist '(font . "Source Code Pro"))
              (set-face-attribute 'default nil :font "Source Code Pro"))))
