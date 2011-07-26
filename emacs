(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/")

(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)

(setq auto-mode-alist
   (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; Insert four spaces on tab
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; org-mode

(setq org-directory "~/Dropbox/org/")

(setq org-latex-to-pdf-process '("/opt/local/bin/pdflatex %f"))

;; (add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'flyspell-mode)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode)) 
(add-hook 'org-mode-hook 'turn-on-font-lock) ; not needed when global-font-lock-mode is on 
(global-set-key "\C-cl" 'org-store-link) 
(global-set-key "\C-ca" 'org-agenda) 
(global-set-key "\C-cb" 'org-iswitchb)

;; use clean view
(setq org-hide-leading-stars t) 

;; add timestamps to state changes for TODOs
(setq org-todo-keywords
       '((sequence "TODO(t!)" "WAIT(w@/!)" "|" "DONE(d@/!)" "CANCELED(c@)")))

;; add files for refiling

(setq org-refile-targets 
      (quote (("personal.org" :maxlevel . 1) 
	      ("wgen.org" :maxlevel . 3))))

;; for capturing notes
(setq org-default-notes-file 
      (concat org-directory "/personal.org")) 

(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates 
      '(("t" "Todo" entry (file+headline (concat org-directory "/personal.org") "Tasks")
	     "* TODO %?\n %i\n %a") 
        ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
	     "* %?\nEntered on %U\n %i\n %a")))

(require 'color-theme)
(color-theme-initialize)
(color-theme-solarized-dark)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Dropbox/org/wgen.org" "~/Dropbox/org/personal.org")))
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))

(set-default-font "-apple-Inconsolata-medium-normal-normal-*-18-*-*-*-m-0-iso10646-1")

(server-start)
