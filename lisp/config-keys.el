;;; -*- lexical-binding: t; -*-
;; Global and leader bindings. Meow's leader keymap is `mode-specific-map',
;; so SPC <group> <key> runs C-c <group> <key>. Groups mirror the Neovim
;; <Leader> groups. Keep C-c j and C-c k unbound (see config-meow.el).

(defun my/open-init ()
  "Open init.el."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(bind-keys
 ("C-x b" . consult-buffer)
 ("C-x C-r" . consult-recent-file)
 ("C-s" . consult-line)
 ("M-y" . consult-yank-pop)
 ("C-c a" . embark-act))

(bind-keys :prefix-map my/buffer-map :prefix "C-c b"
           ("b" . consult-buffer)
           ("d" . kill-current-buffer)
           ("s" . scratch-buffer))

(bind-keys :prefix-map my/explore-map :prefix "C-c e"
           ("d" . dired-jump)
           ("i" . my/open-init))

(bind-keys :prefix-map my/find-map :prefix "C-c f"
           ("f" . project-find-file)
           ("b" . consult-buffer)
           ("g" . consult-ripgrep)
           ("l" . consult-line-multi)
           ("L" . consult-line)
           ("o" . consult-outline)
           ("S" . consult-imenu)
           ("d" . consult-flymake)
           ("v" . consult-recent-file)
           ("r" . vertico-repeat))

(bind-keys :prefix-map my/git-map :prefix "C-c g"
           ("g" . magit-status)
           ("l" . magit-log-buffer-file)
           ("b" . magit-blame-addition)
           ("d" . magit-diff-buffer-file)
           ("s" . diff-hl-show-hunk))

(bind-keys :prefix-map my/lang-map :prefix "C-c l"
           ("a" . eglot-code-actions)
           ("r" . eglot-rename)
           ("f" . eglot-format-buffer)
           ("d" . flymake-show-buffer-diagnostics)
           ("i" . eglot-find-implementation)
           ("t" . eglot-find-typeDefinition)
           ("s" . xref-find-definitions)
           ("R" . xref-find-references)
           ("h" . eldoc-doc-buffer))

(bind-keys :prefix-map my/notes-map :prefix "C-c n"
           ("a" . org-agenda)
           ("c" . org-capture)
           ("s" . org-store-link)
           ("k" . org-insert-link-global)
           ("f" . org-roam-node-find)
           ("i" . org-roam-node-insert)
           ("l" . org-roam-buffer-toggle)
           ("j" . org-roam-dailies-capture-today)
           ("J" . org-roam-dailies-goto-today))

(bind-keys :prefix-map my/terminal-map :prefix "C-c t"
           ("t" . vterm))

(which-key-add-key-based-replacements
  "C-c a" "embark"
  "C-c b" "buffer"
  "C-c e" "explore/edit"
  "C-c f" "find"
  "C-c g" "git"
  "C-c l" "language"
  "C-c n" "notes"
  "C-c t" "terminal")

(provide 'config-keys)
