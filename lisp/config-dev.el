;;; -*- lexical-binding: t; -*-
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))


(setq treesit-font-lock-level 4)
;; Emacs 31: elisp has no tree-sitter. This colors function calls and variables by role.
(setq elisp-fontify-semantically t)

(use-package apheleia
  :ensure t
  :config
  ;; apheleia has no odinfmt entry. odinfmt -stdin reads stdin, writes stdout.
  (setf (alist-get 'odinfmt apheleia-formatters) '("odinfmt" "-stdin"))
  (setf (alist-get 'odin-ts-mode apheleia-mode-alist) 'odinfmt)
  (apheleia-global-mode 1))

(setq require-final-newline t)
(add-hook 'prog-mode-hook
          (lambda () (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))
(use-package odin-ts-mode
  :vc (:url "https://github.com/Sampie159/odin-ts-mode")
  :mode "\\.odin\\'")

(use-package magit
  :ensure t
  :bind ("C-x g" . 'magit-status))

(use-package diff-hl
  :ensure t
  :hook ((prog-mode . diff-hl-mode)
         (magit-pre-refresh . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  (fringe-mode '(8 . 8)))

(use-package eglot
  :hook (prog-mode . eglot-ensure)
  :bind (("C-c e r" . 'eglot-rename)
         ("C-c e f" . 'eglot-format-buffer))
  :custom
  (eglot-send-changes-idle-time 0.1)
  (eglot-extend-to-xref t)              ; activate Eglot in referenced non-project files

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
                                        ; (add-to-list 'eglot-server-programs
                                        ;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode c-or-c++-mode)
                 . ("clangd"
                    "--background-index"
                    "--clang-tidy"
                    "--function-arg-placeholders"
                    "--all-scopes-completion"
                    "--cross-file-rename"
                    "--completion-style=detailed"
                    "--header-insertion=iwyu"
                    "--header-insertion-decorators"))
               )
  (add-to-list 'eglot-server-programs
               '((odin-mode odin-ts-mode) . ("ols")))
  )

;; Built-in project.el (C-x p) replaces projectile; electric-pair replaces smartparens.
(add-hook 'prog-mode-hook #'electric-pair-local-mode)

(defun my/vterm-setting ()
  "Settings for vterm after vterm is active"
  (setq confirm-kill-processes nil
        hscroll-margin 0))

(use-package vterm
  :ensure t
  :bind ("C-c t" . vterm)
  :hook (vterm-mode . my/vterm-setting)
  :config
  (add-to-list 'display-buffer-alist
               '("^\\*vterm"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (slot . -1)
                 (window-height . 0.33)
                 (window-vscroll . t)
                 (window-parameters
                  (no-delete-other-window . nil))))
  (add-to-list 'meow-mode-state-list
               '(vterm-mode . insert))
  (setq vterm-kill-buffer-on-exit t
        vterm-max-scrollback 5000))

(use-package zig-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.\\(zig\\|zon\\)\\'" . zig-mode)))

;; Built in on Emacs 31 but not autoloaded, so treesit-auto skips it; :mode creates the autoload.
(when (< emacs-major-version 31)
  (use-package markdown-ts-mode :ensure t))
(use-package markdown-ts-mode
  :ensure nil
  :mode ("\\.md\\'" . markdown-ts-mode))

(provide 'config-dev)
