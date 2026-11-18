;;; -*- lexical-binding: t; -*-
;; Org-mode, agenda, and org-roam.

(use-package org
  :custom
  (org-directory "~/Documents/org/")    ; relative agenda and capture paths resolve here
  (org-agenda-files '("inbox.org" "work.org"))
  (org-tag-alist '(;; locale
                   (:startgroup)
                   ("home" . ?h)
                   ("work" . ?w)
                   ("school" . ?s)
                   (:endgroup)
                   (:newline)
                   ;; scale
                   (:startgroup)
                   ("one-shot" . ?o)
                   ("project" . ?j)
                   ("tiny" . ?t)
                   (:endgroup)
                   ;; misc
                   ("meta")
                   ("review")
                   ("reading")))
  (org-todo-keywords
   '((sequence "TODO(t)" "WAITING(w@/!)" "STARTED(s!)" "|" "DONE(d!)" "OBSOLETE(o@)")))
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-export-with-smart-quotes t)
  (org-refile-targets '((org-agenda-files :maxlevel . 2)))
  (org-capture-templates
   '(("c" "Default Capture" entry (file "inbox.org")
      "* TODO %?\n%U\n%i")
     ;; Keep an org-link to the thing we are working with.
     ("r" "Capture with Reference" entry (file "inbox.org")
      "* TODO %?\n%U\n%i\n%a")
     ("w" "Work")
     ("wm" "Work meeting" entry (file+headline "work.org" "Meetings")
      "** TODO %?\n%U\n%i\n%a")
     ("wr" "Work report" entry (file+headline "work.org" "Reports")
      "** TODO %?\n%U\n%i\n%a")))
  (org-agenda-custom-commands
   '(("n" "Agenda and All Todos" ((agenda) (todo)))
     ("w" "Work" agenda "" ((org-agenda-files '("work.org"))))))
  ;; Looks. Each setting is a variable, so it costs no redraw work.
  (org-ellipsis " ▾")
  (org-pretty-entities t)               ; \alpha -> α, a_b -> subscript
  (org-startup-indented t)              ; org-indent-mode
  :hook ((org-mode . visual-line-mode)  ; wrap lines at word breaks
         (org-mode . flyspell-mode))
  :config
  (require 'oc-csl)                     ; citation support
  (add-to-list 'org-export-backends 'md)
  ;; Follow file links in the same window.
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file))

;; Needs hunspell and its en_US dictionary on PATH, or flyspell-mode fails.
(setq ispell-program-name "hunspell"
      ispell-dictionary "en_US")

;; --- Org-roam ----------------------------------------------------------------

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/Documents/org-roam/")
  (org-roam-dailies-capture-templates
   '(("d" "log" entry "* %<%H:%M> %?"
      :target (file+head "%<%Y-%m-%d>.org"
                "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-capture-templates
   '(("c" "concept" plain "%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         "#+title: ${title}\n#+filetags: :concept:\n")
      :unnarrowed t)
     ("l" "literature" plain "* Notes\n%?"
      :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                         ":PROPERTIES:\n:ROAM_REFS: ${ref}\n:END:\n#+title: ${title}\n#+filetags: :literature:\n")
       :unnarrowed t)))
  (org-roam-node-display-template
   (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :config
  (org-roam-db-autosync-mode)
  ;; Dedicated side window for backlinks
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (window-width . 0.4)
                 (window-height . fit-window-to-buffer))))

(provide 'config-org)
