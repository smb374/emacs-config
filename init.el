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

(when (< emacs-major-version 29)
  (error (format "Only works with Emacs 29 and newer; you have version ~a" emacs-major-version)))

;; --- Typography stack -------------------------------------------------------

(set-face-attribute 'default nil
                    :height 120 :weight 'regular :family "Maple Mono NF CN")
(set-face-attribute 'bold nil :weight 'bold)
(set-face-attribute 'bold-italic nil :weight 'bold :slant 'italic)
(set-display-table-slot standard-display-table 'truncation (make-glyph-code ?…))
(set-display-table-slot standard-display-table 'wrap (make-glyph-code ?–))

;; --- Frame / windows layout & behavior --------------------------------------
(setq default-frame-alist
      '((height . 44) (width  . 81) (left-fringe . 0) (right-fringe . 0)
        (internal-border-width . 32) (vertical-scroll-bars . nil)
        (bottom-divider-width . 0) (right-divider-width . 0)
        (undecorated-round . t)))
(modify-frame-parameters nil default-frame-alist)
(setq-default pop-up-windows nil)

;; --- Activate / Deactivate modes --------------------------------------------
(tool-bar-mode -1)
(menu-bar-mode -1)
(blink-cursor-mode -1)
(global-hl-line-mode 1)
(pixel-scroll-precision-mode 1)
(savehist-mode 1)
(cua-mode 1)
(recentf-mode 1)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(when (display-graphic-p)
  (context-menu-mode 1))
(which-key-mode 1)

;; --- Backup file settings ---------------------------------------------------
(defun bedrock--backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* ((backupRootDir "~/.emacs.d/emacs-backup/")
         (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
         (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath))
(setopt make-backup-file-name-function 'bedrock--backup-file-name)

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
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq tab-always-indent 'complete)
(defun display-startup-echo-area-message ()
  "If it wasn't for this you'd be GNU/Spammed by now"
  (message ""))
(setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin
(add-hook 'text-mode-hook 'visual-line-mode)

;; --- OSX Specific -----------------------------------------------------------
(when (eq system-type 'darwin)
  (select-frame-set-input-focus (selected-frame))
  (setq mac-option-modifier nil
        ns-function-modifier 'super
        mac-right-command-modifier 'hyper
        mac-right-option-modifier 'alt
        mac-command-modifier 'meta))

;; --- Minibuffer setup -------------------------------------------------------
(defun nano-minibuffer--setup ()
  (set-window-margins nil 3 0)
  (let ((inhibit-read-only t))
    (add-text-properties (point-min) (+ (point-min) 1)
                         `(display ((margin left-margin)
                                    ,(format "# %s" (substring (minibuffer-prompt) 0 1))))))
  (setq truncate-lines t))
(add-hook 'minibuffer-setup-hook #'nano-minibuffer--setup)

;; --- Extra configs ----------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/lisp/")

(require 'config-basic)
(require 'config-dev)
(require 'config-org)
(require 'config-ui)
(require 'config-theme)
