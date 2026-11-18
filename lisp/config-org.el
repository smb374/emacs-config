;;; -*- lexical-binding: t; -*-
;; Org-mode, agenda, and org-roam.

(use-package org
  :custom
  (org-directory "~/Documents/org/")    ; relative agenda and capture paths resolve here
  ;; GTD: inbox.org captures, gtd.org holds Actions/Projects/Waiting,
  ;; someday.org is deliberately outside the agenda.
  (org-agenda-files '("inbox.org" "gtd.org"))
  (org-tag-alist '(;; area
                   (:startgroup)
                   ("home" . ?h)
                   ("work" . ?w)
                   ("school" . ?s)
                   (:endgroup)
                   (:newline)
                   ;; context
                   ("@office" . ?o)
                   ("@errands" . ?e)
                   (:newline)
                   ;; misc
                   ("meta")
                   ("review")
                   ("reading")))
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "PROJ(p)" "WAITING(w@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))
  (org-stuck-projects '("/PROJ" ("NEXT") nil ""))
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-export-with-smart-quotes t)
  (org-refile-targets '(("gtd.org" :maxlevel . 3)
                        ("someday.org" :maxlevel . 2)))
  ;; New archive file each year; the year is fixed when Emacs starts.
  (org-archive-location (concat "archive/%s-" (format-time-string "%Y") "::"))
  ;; Inbox items stay stateless until clarified.
  (org-capture-templates
   '(("c" "Capture" entry (file "inbox.org")
      "* %?\n%U\n%i")
     ;; Keep an org-link to the thing we are working with.
     ("r" "Capture with reference" entry (file "inbox.org")
      "* %?\n%U\n%i\n%a")))
  (org-agenda-custom-commands
   '(("n" "Agenda and All Todos" ((agenda) (todo)))
     ("d" "Daily"
      ((agenda "" ((org-agenda-span 'day)))
       (todo "NEXT" ((org-agenda-sorting-strategy '(tag-up priority-down))))))
     ("r" "Weekly review"
      ((tags "LEVEL=1" ((org-agenda-files '("inbox.org"))
                        (org-agenda-overriding-header "Inbox")))
       (stuck "" ((org-agenda-overriding-header "Stuck projects")))
       (todo "WAITING" ((org-agenda-overriding-header "Waiting for")))
       (todo "" ((org-agenda-files '("someday.org"))
                 (org-agenda-overriding-header "Someday/Maybe")))))
     ("w" "Work"
      ((agenda "") (todo "NEXT"))
      ((org-agenda-tag-filter-preset '("+work"))))))
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

(defun my/org-archive-done ()
  "Archive all DONE and CANCELLED entries in the current file."
  (interactive)
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (org-element-property :begin (org-element-at-point))))
   "/DONE|CANCELLED" 'file))

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
