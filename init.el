;;; -*- lexical-binding: t; -*-

(when (< emacs-major-version 30)
  (error "Only works with Emacs 30 and newer; you have version %d" emacs-major-version))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)


;; --- Activate / Deactivate modes --------------------------------------------
(savehist-mode 1)
(recentf-mode 1)
(setq auto-save-default nil)
(which-key-mode 1)

;; --- Backup file settings ---------------------------------------------------
(defun my--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir (expand-file-name "emacs-backup/" user-emacs-directory))
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) t)
    backupFilePath))
(setopt make-backup-file-name-function 'my--backup-file-name)

;; --- Minimal key bindings ---------------------------------------------------
(defun nano-quit ()
  "Quit minibuffer from anywhere (code from Protesilaos Stavrou)"

  (interactive)
  (cond ((region-active-p) (keyboard-quit))
        ((derived-mode-p 'completion-list-mode) (delete-completion-window))
        ((> (minibuffer-depth) 0) (abort-recursive-edit))
        (t (keyboard-quit))))

(defun nano-kill ()
  "Delete frame or kill emacs if there is only one frame left"

  (interactive)
  (condition-case nil
      (delete-frame)
    (error (save-buffers-kill-terminal))))

(bind-key "C-x k" #'kill-current-buffer)
(bind-key "C-x C-c" #'nano-kill)
(bind-key "C-g" #'nano-quit)
(bind-key "M-n" #'make-frame)
(bind-key "C-z"  nil) ;; No suspend frame
(bind-key "C-<wheel-up>" nil) ;; No text resize via mouse scroll
(bind-key "C-<wheel-down>" nil) ;; No text resize via mouse scroll
(bind-key "C-x ;" #'comment-line)

;; --- Sane settings ----------------------------------------------------------
(set-default-coding-systems 'utf-8)
(setq-default indent-tabs-mode nil
              ring-bell-function 'ignore
              select-enable-clipboard t)
(setq scroll-conservatively 101
      scroll-margin 3
      scroll-preserve-screen-position t
      auto-window-vscroll nil
      backward-delete-char-untabify-method 'hungry)

;; --- Performance ------------------------------------------------------------
(setq gc-cons-threshold (* 100 1024 1024)   ; default 800 KB
      read-process-output-max (* 1024 1024)) ; default 64 KB; larger LSP messages
(global-so-long-mode 1)
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq tab-always-indent 'complete)
(defun display-startup-echo-area-message ()
  "If it wasn't for this you'd be GNU/Spammed by now"
  (message ""))
(add-hook 'text-mode-hook 'visual-line-mode)

;; --- OSX Specific -----------------------------------------------------------
(when (eq system-type 'darwin)
  (select-frame-set-input-focus (selected-frame))
  (setq mac-option-modifier nil
        ns-function-modifier 'super
        mac-right-command-modifier 'hyper
        mac-right-option-modifier 'alt
        mac-command-modifier 'meta))

;; --- Tree-Sitter sources ----------------------------------------------------
(defvar treesit-language-source-alist nil)
(add-to-list 'treesit-language-source-alist '(odin "https://github.com/tree-sitter-grammars/tree-sitter-odin"))
(add-to-list 'treesit-language-source-alist '(markdown-inline "https://github.com/tree-sitter-grammars/tree-sitter-markdown" "split_parser" "tree-sitter-markdown-inline/src"))

;; --- Extra configs ----------------------------------------------------------
(add-to-list 'load-path (expand-file-name "lisp/" user-emacs-directory))

(require 'config-basic)
(require 'config-meow)
(require 'config-dev)
(require 'config-org)
(require 'config-ui)
(require 'config-theme)
(require 'config-keys)
