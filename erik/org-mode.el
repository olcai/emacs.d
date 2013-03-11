;; Org-contacts is really useful
;(require 'org-contacts)
;(setq org-contacts-files
;      (quote ("~/sync/org/contacts.org" "~/sync/org/work/contacts.org")))

;; Files
(setq org-directory "~/sync/org")
(setq org-agenda-files (quote ("~/sync/org" "~/sync/org/work")))

;; Set default note file for capture
(setq org-default-notes-file "~/sync/org/refile.org")

;; Timestamp TODO items when done
(setq org-log-done t)

(setq org-enforce-todo-dependencies t)
(setq org-fast-tag-selection-single-key t)

;; TODO states
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
              (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

;; Set some proper faces for TODO states
(setq org-todo-keyword-faces
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "deep sky blue" :weight bold)
              ("DONE" :foreground "lime green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("HOLD" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "lime green" :weight bold))))

;; Capture templates
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/sync/org/refile.org") "* TODO %?
  %U
  %a

  %i")
              ("n" "note" entry (file "~/sync/org/refile.org") "* %? :NOTE:
  %U
  %a

  %i")
              ("e" "ekonomi todo" entry (file "~/sync/org/ekonomi.org") "* TODO %? %^g
  DEADLINE: %^t
  %a

  %i"))))

;; Refiling
(setq org-completion-use-ido t)
(setq org-outline-path-complete-in-steps t)
(setq org-refile-targets
      (quote ((org-agenda-files :maxlevel . 1) (nil :maxlevel . 1))))
(setq org-refile-use-outline-path (quote file))
(setq org-refile-allow-creating-parent-nodes (quote confirm))

;; Agenda
(setq org-agenda-include-diary t)
(setq org-deadline-warning-days 5)

;; Babel
(setq org-src-fontify-natively t)
(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (awk . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (perl . t)
         (sql . t)
         (gnuplot . t)
         (sh . t)
         (ledger . t)
         (org . t)
         (latex . t))))

(provide 'my-org-mode)
