;;; dev-config.el -*- lexical-binding: t; -*-

;;
;; Development tools and LSP configuration
;;

;; Go LSP configuration
(after! lsp-mode
  ;; Performance basics (lsp-idle-delay set in performance-config.el)
  (setq lsp-use-plists nil
        lsp-log-io nil
        lsp-keep-workspace-alive nil
        lsp-response-timeout 10
        lsp-print-performance nil)

  ;; Suppress semgrep notification warnings
  (setq lsp-warn-no-matched-clients nil)

  (setq lsp-go-use-gofumpt nil
        lsp-go-directory-filters ["-bazel-bin" "-bazel-out" "-bazel-testlogs" "-bazel-mypkg"]
        lsp-go-use-placeholders t
        lsp-go-codelenses '((gc_details . :json-false)
                            (generate . :json-false)
                            (regenerate_cgo . :json-false)
                            (tidy . :json-false)
                            (upgrade_dependency . :json-false)
                            (vendor . :json-false)))

  ;; General LSP file watching configuration
  (setq lsp-file-watch-ignored-directories
        (append lsp-file-watch-ignored-directories
                '("[/\\\\]bazel-bin$" "[/\\\\]bazel-out$" "[/\\\\]bazel-testlogs$"
                  "[/\\\\]bazel-mypkg$"
                  "[/\\\\]node_modules$" "[/\\\\]\\.git$" "[/\\\\]vendor$"
                  "[/\\\\]target$" "[/\\\\]build$" "[/\\\\]\\.gradle$" "[/\\\\]\\.idea$"))
        lsp-file-watch-threshold 2000
        lsp-enable-file-watchers nil)

  ;; Gopls memory optimization settings
  (lsp-register-custom-settings
   '(("gopls.staticcheck" nil t)
     ("gopls.semanticTokens" nil t)
     ("gopls.analyses.fillstruct" nil t)
     ("gopls.analyses.nonewvars" nil t)
     ("gopls.analyses.nolintlint" nil t)
     ("gopls.analyses.undeclaredname" nil t)
     ("gopls.analyses.unusedparams" nil t)
     ("gopls.analyses.unusedwrite" nil t)
     ("gopls.completeUnimported" nil t)
     ("gopls.completionDocumentation" nil t)
     ("gopls.deepCompletion" nil t)
     ("gopls.matcher" "Fuzzy" t)
     ("gopls.usePlaceholders" nil t))))

;; Configure file watching exclusions for large repositories
(after! projectile
  (setq projectile-file-exists-remote-cache-expire nil
        projectile-globally-ignored-directories
        (append projectile-globally-ignored-directories
                '("bazel-bin" "bazel-out" "bazel-testlogs" "bazel-mypkg"
                  "node_modules" ".git" "vendor" "target" "build" ".gradle" ".idea"))
        ;; Exclude compressed files from project search
        projectile-globally-ignored-file-suffixes
        (append projectile-globally-ignored-file-suffixes
                '("gz" "zip" "tar" "bz2" "xz" "7z" "rar" "tgz")))

  ;; Reduce file watching for large projects
  (setq projectile-enable-caching t
        projectile-project-root-files-bottom-up '(".projectile" ".git" ".hg" ".bzr" "_darcs")
        projectile-project-root-files-top-down-recurring '("Makefile" "setup.py" "pom.xml" "build.gradle")))

;; Configure consult to exclude compressed files from search
(after! consult
  (setq consult-fd-args
        (append consult-fd-args
                '("--exclude=*.gz" "--exclude=*.zip" "--exclude=*.tar"
                  "--exclude=*.bz2" "--exclude=*.xz" "--exclude=*.7z"
                  "--exclude=*.rar" "--exclude=*.tgz"))))

;; Claude Code IDE configuration
(use-package! claude-code-ide
  :bind ("C-c C-'" . claude-code-ide-menu)
  :config
  (claude-code-ide-emacs-tools-setup)
  ;; Use vterm as the terminal backend
  (setq claude-code-ide-terminal-backend 'vterm
        claude-code-ide-window-side 'right))

(provide 'dev-config)