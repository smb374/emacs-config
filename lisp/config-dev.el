(use-package treesit-auto
  :straight t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package magit
  :straight t
  :bind ("C-x g" . 'magit-status))

(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
                                        ; (add-to-list 'eglot-server-programs
                                        ;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  )

(use-package projectile
  :straight t
  :bind ("C-c p" . 'projectile-command-map)
  :init
  (setq projectile-switch-project-action 'neotree-projectile-action)
  :config
  (projectile-mode 1))

(use-package smartparens
  :straight t
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config))

(provide 'config-dev)
