;;; -*- lexical-binding: t; -*-
;; Basic extensions
(use-package nerd-icons
  :ensure t)

(editorconfig-mode 1) ; built in

(use-package indent-bars
  :ensure t
  :hook (prog-mode . indent-bars-mode))

(use-package rainbow-mode
  :ensure t
  :hook ((css-mode css-ts-mode html-mode html-ts-mode mhtml-mode
          conf-mode toml-ts-mode yaml-ts-mode) . rainbow-mode))

;; (use-package rainbow-delimiters
;;   :ensure t
;;   :hook (prog-mode . rainbow-delimiters-mode))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

;; Motion aids
(use-package avy
  :ensure t
  :demand t
  :bind (("C-c j" . avy-goto-line)
         ("S-j" . avy-goto-char-timer)))

;; Consult: Misc. enhanced commands
(use-package consult
  :ensure t
  :bind (("C-x b" . 'consult-buffer)
         ("M-y" . 'consult-yank-pop)
         ("C-s" . 'consult-line)
         ("C-c b" . 'consult-buffer)
         ("C-c s" . 'consult-line)
         ("C-c r" . 'consult-recent-file)
         ("C-c k" . 'consult-ripgrep)
         ("C-c L" . 'consult-line-multi)
         ("C-c o" . 'consult-outline))
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))

(use-package embark
  :ensure t
  :after avy
  :bind (("C-c a" . embark-act))        ; bind this to an easy key to hit
  :init
  ;; Add the option to run embark when using avy
  (defun my/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'my/avy-action-embark))

(use-package embark-consult
  :ensure t)

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :ensure t
  :init
  ;; You'll want to make sure that e.g. fido-mode isn't enabled
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

;; Corfu
(use-package corfu
  :ensure t
  :bind
  (:map corfu-map
        ("TAB" . 'corfu-next)
        ([tab] . 'corfu-next)
        ("S-TAB" . 'corfu-previous)
        ([backtab] . 'corfu-previous)
        ([escape] . 'corfu-quit))
  :init
  (global-corfu-mode)
  (corfu-history-mode t)
  (corfu-popupinfo-mode t)
  (setq corfu-auto t
        corfu-auto-delay 0.1
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-preselect 'prompt
        corfu-count 16
        corfu-max-width 120
        corfu-on-exact-match nil
        corfu-popupinfo-delay 0.3
        completion-cycle-threshold nil
        corfu-preselect-first nil))

(use-package corfu-terminal
  :if (and (not (display-graphic-p)) (< emacs-major-version 31))
  :ensure t
  :config
  (corfu-terminal-mode))

;; Pretty icons for corfu
(use-package nerd-icons-corfu
  :ensure t
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Fancy completion-at-point functions; there's too much in the cape package to
;; configure here; dive in when you're comfortable!
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

(use-package eshell
  :init
  (defun my/setup-eshell ()
    ;; Something funny is going on with how Eshell sets up its keymaps; this is
    ;; a work-around to make C-r bound in the keymap
    (keymap-set eshell-mode-map "C-r" 'consult-history))
  :hook ((eshell-mode . my/setup-eshell)))

;; Orderless: powerful completion style
(use-package orderless
  :ensure t
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; Modify search results en masse
(use-package wgrep
  :ensure t
  :config
  (setq wgrep-auto-save-buffer t))


(provide 'config-basic)
