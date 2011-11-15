(add-to-list 'load-path "/Users/tbutt/code/3p/emacs-color-theme-solarized")

(setq exec-path (append exec-path '("/usr/local/bin")))

;; word count
(defun count-words (start end)
    "Print number of words in the region."
    (interactive "r")
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (count-matches "\\sw+"))))

;; recentf
(require 'recentf)
(recentf-mode 1)

;; spellchecking
(setq-default ispell-program-name "aspell")

;; Insert four spaces on tab
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; org-mode
(setq org-directory "~/org/")

;; (add-hook 'org-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'flyspell-mode)
;; (add-hook 'org-mode-hook 'reftex-mode)

;; use texi2dvi
(setq org-latex-to-pdf-process '("/usr/bin/texi2dvi --pdf --clean --verbose --batch %f"))
(if (eq window-system 'mac)
   (add-to-list 'exec-path "/usr/local/texlive/2011/bin/universal-darwin")
)

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
(setq org-default-notes-file (concat org-directory "/personal.org")) 

(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates 
      '(("t" "Todo" entry (file+headline (concat org-directory "/personal.org") "Tasks")
	     "* TODO %?\n %i\n %a") 
        ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
	     "* %?\nEntered on %U\n %i\n %a")))
 
;; (color-theme-solarized-dark)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(tool-bar-mode nil))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(require 'package)
(add-to-list 'package-archives
             '("josh" . "http://josh.github.com/elpa/") t)

(set-default-font "-apple-Inconsolata-medium-normal-normal-*-18-*-*-*-m-0-iso10646-1")

(global-set-key (kbd "M-F") 'ns-toggle-fullscreen)

;; reftex with orgmode

(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name) (file-exists-p (buffer-file-name))
       (progn
     ;enable auto-revert-mode to update reftex when bibtex file changes on disk
     (global-auto-revert-mode t)
     (reftex-parse-all)
     ;add a custom reftex cite format to insert links
     (reftex-set-cite-format
      '((?b . "[[bib:%l][%l-bib]]")
        (?n . "[[notes:%l][%l-notes]]")
        (?p . "[[papers:%l][%l-paper]]")
        (?c . "\\citep{%l}")
        (?t . "%t")
        (?h . "** %t\n:PROPERTIES:\n:Custom_ID: %l\n:END:\n[[papers:%l][%l-paper]]")))))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  (define-key org-mode-map (kbd "C-c (") 'org-mode-reftex-search))

(add-hook 'org-mode-hook 'org-mode-reftex-setup)

(defun org-mode-reftex-search ()
  ;;jump to the notes for the paper pointed to at from reftex search
  (interactive)
  (org-open-link-from-string (format "[[notes:%s]]" (reftex-citation t))))

(setq org-link-abbrev-alist
      '(("bib" . (concat org-directory "/research/refs.bib::%s"))
        ("notes" . (concat org-directory "/research/notes.org::#%s"))
        ("papers" . (concat org-directory "/research/papers/%s.pdf")))) 

;; start emacs server

(server-start)
