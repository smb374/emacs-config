;;; -*- lexical-binding: t; -*-

;; --- Typography stack -------------------------------------------------------
(set-face-attribute 'default nil
                    :height 110 :weight 'regular :family "Maple Mono NF CN")
(set-face-attribute 'bold nil :weight 'bold)
(set-face-attribute 'bold-italic nil :weight 'bold :slant 'italic)
(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

;; --- Frame / windows layout & behavior --------------------------------------
(setq default-frame-alist
      '((height . 44) (width  . 81)
        (internal-border-width . 32) (vertical-scroll-bars . nil)
        (bottom-divider-width . 0) (right-divider-width . 0)
        (undecorated-round . t)))
(modify-frame-parameters nil default-frame-alist)
(setq-default pop-up-windows nil)

;; --- UI modes ---------------------------------------------------------------
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(global-hl-line-mode 1)
(pixel-scroll-precision-mode 1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(when (display-graphic-p)
  (context-menu-mode 1))
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; --- Minibuffer setup -------------------------------------------------------
(defun my-minibuffer--setup ()
  (set-window-margins nil 3 0)
  (let ((inhibit-read-only t))
    (add-text-properties (point-min) (+ (point-min) 1)
                         `(display ((margin left-margin)
                                    ,(format "# %s" (substring (minibuffer-prompt) 0 1))))))
  (setq truncate-lines t))
(add-hook 'minibuffer-setup-hook #'my-minibuffer--setup)

;; --- Packages ---------------------------------------------------------------
(use-package neotree
  :straight t
  :bind ([f8] . 'neotree-toggle)
  :config
  (setq neo-theme (if (display-graphic-p) 'nerd-icons 'ascii)))

(use-package doom-modeline
  :straight t
  :hook (after-init . doom-modeline-mode))

(provide 'config-ui)
