;;; config.el --- description -*- lexical-binding: t; -*-

(setq user-mail-address "tahir@tahirbutt.com"
      user-full-name    "Tahir H. Butt"

      doom-themes-enable-bold t
      doom-themes-enable-italic t
      doom-font (font-spec :family "IBM Plex Mono" :size 16)
      doom-variable-pitch-font (font-spec :family "IBM Plex Sans" :size 14)
      doom-unicode-font (font-spec :family "Source Code Pro" :size 16)
      doom-big-font (font-spec :family "IBM Plex Mono" :size 20)
      doom-line-numbers-style nil
      ivy-posframe-font (font-spec :family "Input Mono Narrow" :size 16)
      ivy-height 12
      +rss-elfeed-files '("elfeed.org")

      org-ellipsis " + "

      +write-text-scale 1.5
      doom-theme 'doom-nord
      )


(after! ox-pandoc
  (setq org-pandoc-options
        '((standalone . t)
          (mathjax . t)))
  (setq org-pandoc-options-for-latex-pdf
        '((pdf-engine . "xelatex"))))

(setq +org-dir (expand-file-name "~/Dropbox/org/"))

(after! org
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline +org-default-todo-file "Inbox")
           "* TODO %?\n %i\n %a" :prepend t :kill-buffer t)

          ("j" "Journal" entry
           (file+olp+datetree "journal.org" "Inbox")
           "* %?\nEntered on %U\n %i\n %a" :prepend t :kill-buffer t)

          ("n" "Notes" entry
           (file+headline +org-default-notes-file "Inbox")
           "* %u %?\n %i" :prepend t :kill-buffer t))))

(def-package! ace-link
  :commands (ace-link ace-link-eww ace-link-mu4e ace-link-elfeed))

(def-package! org-zotxt
  :commands org-zotxt-mode
  :init (add-hook 'org-mode-hook #'org-zotxt-mode)
  :config
  (setq org-zotxt-default-search-method :everything)
  (setq org-zotxt-link-description-style :betterbibtexkey))

(after! shr
  (require 'shr-tag-pre-highlight)
  (add-to-list 'shr-external-rendering-functions
               '(pre . shr-tag-pre-highlight)))

(after! magithub
  (setq magithub-clone-default-directory (expand-file-name "~/GitHub/")))

(after! elfeed-show
  (map! (:map elfeed-show-mode-map
          [remap kill-this-buffer]      "q"
          [remap kill-buffer]           "q"
          :nm "q"   #'+rss/delete-pane
          :nm "o"   #'ace-link-elfeed
          :nm "RET" #'org-ref-add-bibtex-entry-from-elfeed-entry
          :nm "n"   #'elfeed-show-next
          :nm "p"   #'elfeed-show-prev
          :nm "+"   #'elfeed-show-tag
          :nm "-"   #'elfeed-show-untag
          :nm "s"   #'elfeed-show-new-live-search
          :nm "y"   #'elfeed-show-yank)))

(after! mu4e
  (require 'mu4e-contrib)

  (setq mu4e-get-mail-command "mbsync --all --new --renew --delete --flags --pull --push --expunge --verbose")

  (setq mu4e-html2text-command 'mu4e-shr2text
        shr-width 100
        mu4e-sent-messages-behavior 'delete
        mu4e-use-fancy-chars t
        shr-color-visible-distance-min 10
        shr-color-visible-luminance-min 80)


  (setq mu4e-bookmarks
        `(("maildir:/Inbox/" "Inbox" ?i)
          ("maildir:/Draft/" "Drafts" ?d)
          ("flag:flagged" "Starred messages" ?s)
          ("flag:unread" "Unread messages" ?u)
          ("flag:unread AND flag:l" "Unread list messages" ?l)
          ("date:7d..now" "Last 7 days" ?w)))

  (setq smtpmail-stream-type 'starttls
        smtpmail-default-smtp-server "smtp-mail.outlook.com"
        smtpmail-smtp-server "smtp-mail.outlook.com"
        smtpmail-smtp-service 587)

  (set! :email "gc"
    '((mu4e-sent-folder       . "/gc/Sent")
      (mu4e-drafts-folder     . "/gc/Drafts")
      (mu4e-trash-folder      . "/gc/Deleted")
      (mu4e-refile-folder     . "/gc/Archive")
      (smtpmail-smtp-user     . "tbutt@gradcenter.cuny.edu")
      (user-mail-address      . "tbutt@gradcenter.cuny.edu")))

  (set! :email "outlook"
    '((mu4e-sent-folder       . "/outlook/Sent")
      (mu4e-drafts-folder     . "/outlook/Drafts")
      (mu4e-trash-folder      . "/outlook/Deleted")
      (mu4e-refile-folder     . "/outlook/Archive")
      (smtpmail-smtp-user     . "tahir@tahirbutt.com")
      (user-mail-address      . "tahir@tahirbutt.com")) t)

  (add-hook 'mu4e-index-updated-hook
	    (defun update()
          (mu4e-maildirs-extension-force-update '(16))))

  (setq mu4e-view-mode-map (make-sparse-keymap)
        ;; mu4e-compose-mode-map (make-sparse-keymap)
        mu4e-headers-mode-map (make-sparse-keymap)
        mu4e-main-mode-map (make-sparse-keymap))

  (map! (:map (mu4e-main-mode-map mu4e-view-mode-map)
          :leader
          :n "," #'mu4e-context-switch
          :n "." #'mu4e-headers-search-bookmark
          :n "/" #'mu4e-headers-search-edit
          :n ">" #'mu4e-headers-search-bookmark-edit)


        (:map (mu4e-headers-mode-map mu4e-view-mode-map)
          :localleader
          :n "f" #'mu4e-compose-forward
          :n "r" #'mu4e-compose-reply
          :n "c" #'mu4e-compose-new
          :n "e" #'mu4e-compose-edit)

        (:map mu4e-main-mode-map
          :n "q"   #'mu4e-quit
          :n "u"   #'mu4e-update-index
          :n "U"   #'mu4e-update-mail-and-index
          :n "J"   #'mu4e~headers-jump-to-maildir
          :n "c"   #'+email/compose
          :n "b"   #'mu4e-headers-search-bookmark)

        (:map mu4e-headers-mode-map
          :n "q"   #'mu4e~headers-quit-buffer
          :n "r"   #'mu4e-compose-reply
          :n "c"   #'mu4e-compose-edit
          :n "s"   #'mu4e-headers-search-edit
          :n "S"   #'mu4e-headers-search-narrow
          :n "RET" #'mu4e-headers-view-message
          :n "u"   #'mu4e-headers-mark-for-unmark
          :n "U"   #'mu4e-mark-unmark-all
          :n "v"   #'evil-visual-line
          :nv "d"  #'+email/mark
          :nv "="  #'+email/mark
          :nv "-"  #'+email/mark
          :nv "+"  #'+email/mark
          :nv "!"  #'+email/mark
          :nv "?"  #'+email/mark
          :nv "r"  #'+email/mark
          :nv "m"  #'+email/mark
          :n "x"   #'mu4e-mark-execute-all

          :n "]]"  #'mu4e-headers-next-unread
          :n "[["  #'mu4e-headers-prev-unread

          (:localleader
            :n "s" 'mu4e-headers-change-sorting
            :n "t" 'mu4e-headers-toggle-threading
            :n "r" 'mu4e-headers-toggle-include-related

            :n "%" #'mu4e-headers-mark-pattern
            :n "t" #'mu4e-headers-mark-subthread
            :n "T" #'mu4e-headers-mark-thread))

        (:map mu4e-view-mode-map
          :n "q" #'mu4e~view-quit-buffer
          :n "r" #'mu4e-compose-reply
          :n "c" #'mu4e-compose-edit
          :n "o" #'ace-link-mu4e
          :n "O" #'mu4e-view-go-to-url
          :n "H" #'mu4e-view-toggle-html

          :n "<M-Left>"  #'mu4e-view-headers-prev
          :n "<M-Right>" #'mu4e-view-headers-next
          :n "[m" #'mu4e-view-headers-prev
          :n "]m" #'mu4e-view-headers-next
          :n "[u" #'mu4e-view-headers-prev-unread
          :n "]u" #'mu4e-view-headers-next-unread

          (:localleader
            :n "%" #'mu4e-view-mark-pattern
            :n "t" #'mu4e-view-mark-subthread
            :n "T" #'mu4e-view-mark-thread

            :n "d" #'mu4e-view-mark-for-trash
            :n "r" #'mu4e-view-mark-for-refile
            :n "m" #'mu4e-view-mark-for-move))

        (:map mu4e~update-mail-mode-map
          :n "q" #'mu4e-interrupt-update-mail)))


(provide 'config)
;;; config.el ends here
