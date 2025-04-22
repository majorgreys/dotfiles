;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq shell-file-name (executable-find "bash"))

(setq
 user-mail-address "tahir@tahirbutt.com"
 user-full-name    "Tahir H. Butt"
 doom-theme 'catppuccin
 catppuccin-flavor 'macchiato
 doom-font (font-spec :family "PragmataPro Mono Liga" :size 14)
 doom-unicode-font (font-spec :family "PragmataPro Mono Liga")
 doom-variable-pitch-font (font-spec :family "Iosevka Aile" :size 14)
 doom-big-font (font-spec :family "PragmataPro Mono Liga" :size 22))

(when IS-MAC
  (setq ns-use-thin-smoothing t)    ; thinner strokes for font-smoothing
  ;; macOS natural title bars
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))

;; lsp
;; (after! lsp-mode
;;   (setq lsp-log-io nil
;;         lsp-file-watch-threshold 4000
;;         lsp-headerline-breadcrumb-enable t
;;         lsp-headerline-breadcrumb-segments '(file symbols)
;;         lsp-imenu-index-symbol-kinds '(File Module Namespace Package Class Method Enum Interface
;;                                        Function Variable Constant Struct Event Operator TypeParameter)
;;         )
;;   (dolist (dir '("[/\\\\]\\.ccls-cache\\'"
;;                  "[/\\\\]\\.mypy_cache\\'"
;;                  "[/\\\\]\\.pytest_cache\\'"
;;                  "[/\\\\]\\.riot\\'"
;;                  "[/\\\\]\\.ddriot\\'"
;;                  "[/\\\\]\\.tox\\'"
;;                  "[/\\\\]\\.ddtox\\'"
;;                  "[/\\\\]\\.cache\\'"
;;                  "[/\\\\]\\.clwb\\'"
;;                  "[/\\\\]__pycache__\\'"
;;                  "[/\\\\]build\\'"
;;                  "[/\\\\]bazel-bin\\'"
;;                  "[/\\\\]bazel-code\\'"
;;                  "[/\\\\]bazel-genfiles\\'"
;;                  "[/\\\\]bazel-out\\'"
;;                  "[/\\\\]bazel-testlogs\\'"
;;                  "[/\\\\]third_party\\'"
;;                  "[/\\\\]third-party\\'"
;;                  "[/\\\\]buildtools\\'"
;;                  "[/\\\\]out\\'"
;;                  "[/\\\\]bin\\'"
;;                  "[/\\\\]ci\\'"
;;                  "[/\\\\]ci-app\\'"
;;                  "[/\\\\]etc\\'"
;;                  "[/\\\\]ops\\'"
;;                  "[/\\\\]spinnaker\\'"
;;                  "[/\\\\]tilt\\'"
;;                  "[/\\\\]vendor\\'"
;;                  "[/\\\\]k8s\\'")))
;;   (push dir lsp-file-watch-ignored-directories))

;; (after! lsp-ui
;;   (setq lsp-ui-doc-enable nil
;;         lsp-lens-enable nil
;;         lsp-ui-sideline-enable nil
;;         lsp-ui-doc-include-signature t
;;         lsp-ui-doc-max-height 15
;;         lsp-ui-doc-max-width 100))

(setq writeroom-fullscreen-effect t)

;; not sure why but projectile-generic-command is not getting set correctly
;; (when (executable-find "rg")
;;   (setq-default projectile-generic-command "rg --files --hidden -0"))

;; https://discord.com/channels/406534637242810369/1178820376818618448/1179167822044205157
;; (setq projectile-indexing-method 'alien)

(provide 'config)
