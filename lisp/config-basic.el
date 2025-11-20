;; Basic extensions
(use-package nerd-icons
  :straight t)

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

(use-package indent-guide
  :straight t
  :hook (prog-mode . indent-guide-mode))

;; Motion aids
(use-package avy
  :straight t
  :demand t
  :bind (("C-c j" . avy-goto-line)
          ("S-j" . avy-goto-char-timer)))

;; Consult: Misc. enhanced commands
(use-package consult
  :straight t
  :bind ( ("C-x b" . 'consult-buffer)
          ("M-y" . 'consult-yank-pop)
          ("C-s" . 'consult-line)
          ("C-c b" . 'consult-buffer)
          ("C-c s" . 'consult-line)
          ("C-c r" . 'consult-recent-file)
          ("C-c k" . 'consult-ripgrep)
          ("C-c l" . 'consult-line-multi)
          ("C-c o" . 'consult-outline))
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))

(use-package embark
  :straight t
  :after avy
  :bind (("C-c a" . embark-act))        ; bind this to an easy key to hit
  :init
  ;; Add the option to run embark when using avy
  (defun bedrock/avy-action-embark (pt)
    (unwind-protect
      (save-excursion
        (goto-char pt)
        (embark-act))
      (select-window
        (cdr (ring-ref avy-ring 0))))
    t)

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark))

(use-package embark-consult
  :straight t)

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :straight t
  :init
  ;; You'll want to make sure that e.g. fido-mode isn't enabled
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
          ("M-DEL" . vertico-directory-delete-word)))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :straight t
  :config
  (marginalia-mode))

;; Corfu
(use-package corfu
  :straight t
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

(use-package corfu-history
  :after corfu
  :hook (corfu-mode . corfu-history-mode))

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :straight t
  :config
  (corfu-terminal-mode))

;; Pretty icons for corfu
(use-package nerd-icons-corfu
  :straight t
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Fancy completion-at-point functions; there's too much in the cape package to
;; configure here; dive in when you're comfortable!
(use-package cape
  :straight t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

(use-package eshell
  :init
  (defun bedrock/setup-eshell ()
    ;; Something funny is going on with how Eshell sets up its keymaps; this is
    ;; a work-around to make C-r bound in the keymap
    (keymap-set eshell-mode-map "C-r" 'consult-history))
  :hook ((eshell-mode . bedrock/setup-eshell)))

;; Orderless: powerful completion style
(use-package orderless
  :straight t
  :custom
  ;; (orderless-style-dispatchers '(orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring

;; Modify search results en masse
(use-package wgrep
  :straight t
  :config
  (setq wgrep-auto-save-buffer t))

;; Meow
(use-package meow
  :straight t
  :init
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-define-key
      '("j" . meow-next)
      '("k" . meow-prev)
      '("<escape>" . ignore))
    (meow-leader-define-key
      ;; Use SPC (0-9) for digit arguments.
      '("1" . meow-digit-argument)
      '("2" . meow-digit-argument)
      '("3" . meow-digit-argument)
      '("4" . meow-digit-argument)
      '("5" . meow-digit-argument)
      '("6" . meow-digit-argument)
      '("7" . meow-digit-argument)
      '("8" . meow-digit-argument)
      '("9" . meow-digit-argument)
      '("0" . meow-digit-argument)
      '("/" . meow-keypad-describe-key)
      '("?" . meow-cheatsheet))
    (meow-normal-define-key
      '("0" . meow-expand-0)
      '("9" . meow-expand-9)
      '("8" . meow-expand-8)
      '("7" . meow-expand-7)
      '("6" . meow-expand-6)
      '("5" . meow-expand-5)
      '("4" . meow-expand-4)
      '("3" . meow-expand-3)
      '("2" . meow-expand-2)
      '("1" . meow-expand-1)
      '("-" . negative-argument)
      '(";" . meow-reverse)
      '("," . meow-inner-of-thing)
      '("." . meow-bounds-of-thing)
      '("[" . meow-beginning-of-thing)
      '("]" . meow-end-of-thing)
      '("a" . meow-append)
      '("A" . meow-open-below)
      '("b" . meow-back-word)
      '("B" . meow-back-symbol)
      '("c" . meow-change)
      '("d" . meow-delete)
      '("D" . meow-backward-delete)
      '("e" . meow-next-word)
      '("E" . meow-next-symbol)
      '("f" . meow-find)
      '("g" . meow-cancel-selection)
      '("G" . meow-grab)
      '("h" . meow-left)
      '("H" . meow-left-expand)
      '("i" . meow-insert)
      '("I" . meow-open-above)
      '("j" . meow-next)
      '("J" . meow-next-expand)
      '("k" . meow-prev)
      '("K" . meow-prev-expand)
      '("l" . meow-right)
      '("L" . meow-right-expand)
      '("m" . meow-join)
      '("n" . meow-search)
      '("o" . meow-block)
      '("O" . meow-to-block)
      '("p" . meow-yank)
      '("q" . meow-quit)
      '("Q" . meow-goto-line)
      '("r" . meow-replace)
      '("R" . meow-swap-grab)
      '("s" . meow-kill)
      '("t" . meow-till)
      '("u" . meow-undo)
      '("U" . meow-undo-in-selection)
      '("v" . meow-visit)
      '("w" . meow-mark-word)
      '("W" . meow-mark-symbol)
      '("x" . meow-line)
      '("X" . meow-goto-line)
      '("y" . meow-save)
      '("Y" . meow-sync-grab)
      '("z" . meow-pop-selection)
      '("'" . repeat)
      '("<escape>" . nano-quit)))
  :config
  (meow-setup)
  (meow-define-keys
    'normal
    ;; Prvent RET & DEL modifying the content in normal state
    '("RET" . meow-next)
    '("DEL" . meow-prev))
  (meow-global-mode 1))

(provide 'config-basic)
