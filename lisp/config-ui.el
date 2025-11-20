(use-package neotree
  :straight t
  :bind ([f8] . 'neotree-toggle)
  :config
  (setq neo-theme (if (display-graphic-p) 'nerd-icons 'ascii)))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode))

(provide 'config-ui)
