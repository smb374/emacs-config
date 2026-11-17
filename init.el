;;; -*- lexical-binding: t; -*-

(when (< emacs-major-version 29)
  (error "Only works with Emacs 29 and newer; you have version %d" emacs-major-version))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'org)
(straight-use-package 'use-package)


;; --- Activate / Deactivate modes --------------------------------------------
(savehist-mode 1)
(cua-mode 1)
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
(bind-key "C-x C-r" #'recentf-open)
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
