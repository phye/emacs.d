;; {{ hooks
(defun phye/org-mode-hook ()
  "custom orgmode settings"
  (interactive)
  (set-fill-column 90)
  (turn-on-auto-fill)
  (org-clock-persistence-insinuate)
  (hl-todo-mode 1)
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-default-header-args:plantuml
        '((:results . "replace")
          (:exports . "results")))
  (setq org-tags-column -80)
  (setq org-catch-invisible-edits (quote error))
  (setq safe-local-variable-values (quote ((lentic-init . lentic-orgel-org-init))))
  (setq org-deadline-warning-days 7)
  (setq org-log-into-drawer t)
  (setq org-clock-persist 'history)
  (setq org-src-window-setup 'split-window-below)
  (setq org-archive-location "archive.org.gpg::datetree/* From %s")
  (setq org-adapt-indentation t)
  (define-key org-mode-map (kbd "C-c o") 'org-open-at-point)
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (my-yas-reload-all))
(add-hook 'org-mode-hook 'phye/org-mode-hook 90)
;; }}

;; {{ Complex Org settings
(with-eval-after-load 'org
  (setq org-tag-alist '((:startgroup . nil) ;; tag group for address
                        ("@work" . ?w)
                        ("@home" . ?h)
                        ("@travel" . ?t)
                        (:endgroup . nil)
                        (:startgroup . nil) ;; tag group for reading
                        ("book" . ?b)
                        ("kindle" . ?k)
                        ("pad" . ?d)
                        ("computer" . ?c)
                        (:endgroup . nil)
                        (:startgroup . nil) ;; tag group for privacy
                        ("personal" . ?p)
                        ("office" . ?o)
                        ("public" . ?a)
                        (:endgroup . nil)
                        (:startgroup . nil) ;; tag group for kind of things
                        ("project" . ?j)
                        ("reading" . ?r)
                        ("ideas" . ?i)
                        ("misc" . ?m)
                        ("financial" . ?f)
                        ("health" . ?h)
                        ("gtd" . ?g)
                        (:endgroup)))

  (setq org-todo-keywords
        '((sequence "TODO(t!/!)" "SCHEDULED(S@/@)" "INPROGRESS(i!/!)" "BLOCKED(b@/@)" "|" "DONE(d)" "DEFERED(f@/@)" "CANCELLED(c@/!)") ;; general todo items
          (sequence "ASSIGNED(a@/!)" "REPRODUCED(p@)" "RCFOUND(r@)" "|" "FIXED(x!)" "VERIFIED(v!)") ;; bug only
          (type "APPT(p)" "REMINDER(m!)" "|" "DONE(d)"))) ;; misc daily items

  (setq org-agenda-files
        (quote
         ("~/ws/gtd/gtd.org"
          "~/ws/gtd/quick_notes.org"
          "~/ws/gtd/life/plan_life.org"
          "~/ws/gtd/life/birthday.org"
          "~/ws/gtd/learn/plan_learning.org"
          "~/ws/gtd/work/plan_work.org.gpg")))

  (setq org-refile-targets
        '((nil :maxlevel . 5)
          (org-agenda-files :maxlevel . 5)
          ("KnowledgeBase.org" :maxlevel . 5)
          ("done.org" :maxlevel . 5)))

  ;; org babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (ditaa . t)
     (plantuml . t)
     (dot . t)
     ))

  ;; Org Mode Capture
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/ws/gtd/gtd.org" "Tasks")
           "* TODO %?\n %i\n %a")
          ("n" "Note" entry (file+datetree "~/ws/gtd/quick_notes.org")
           "* %?\nEntered on %U\n %i\n %a")
          ("j" "Journal entry" entry (function org-journal-find-location)
           "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")))

  ;; OrgMode Output
  (require 'ox-md nil t)
  (require 'ox-odt nil t)
  (setq org-export-with-sub-superscripts nil)
  (setq org-export-with-properties t)

  (with-eval-after-load 'ox-latex
    ;; update the list of LaTeX classes and associated header (encoding, etc.)
    ;; and structure
    (add-to-list 'org-latex-classes
                 `("beamer"
                   ,(concat "\\documentclass[presentation]{beamer}\n"
                            "[DEFAULT-PACKAGES]"
                            "[PACKAGES]"
                            "[EXTRA]\n")
                   ("\\section{%s}" . "\\section*{%s}")
                   ("\\subsection{%s}" . "\\subsection*{%s}")
                   ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

  (setq org-latex-listings t)
  (setq org-latex-to-pdf-process
        '("xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f")))

;; }} Complex Org Settings

;; {{ Org Mode Extensions

;; {{ calendar and org-agenda
(customize-set-variable 'holiday-local-holidays
                        '((holiday-fixed 1 1 "元旦")
                          (holiday-fixed 3 8 "妇女节")
                          (holiday-fixed 5 1 "劳动节")
                          (holiday-fixed 5 4 "青年节")
                          (holiday-fixed 6 1 "儿童节")
                          (holiday-fixed 7 1 "建党节")
                          (holiday-fixed 8 1 "建军节")
                          (holiday-fixed 10 1 "国庆节")
                          ))

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "W") 'org-agenda-week-view)
  (define-key org-agenda-mode-map (kbd "F") 'org-agenda-fortnight-view)
  (define-key org-agenda-mode-map (kbd "M") 'org-agenda-month-view)
  (define-key org-agenda-mode-map (kbd "Y") 'org-agenda-year-view)
  (customize-set-variable 'calendar-chinese-all-holidays-flag t)
  (customize-set-variable 'calendar-holidays
                          (append holiday-oriental-holidays
                                  holiday-local-holidays
                                  holiday-other-holidays)))
(with-eval-after-load 'ox-icalendar
  (customize-set-variable 'org-icalendar-combined-name "OrgMode")
  (customize-set-variable 'org-icalendar-combined-agenda-file "~/ws/gtd/gtd.ics"))
;; }}

;; {{ org-journal related
(use-package org-journal
  :ensure t
  :defer t
  :custom
  (org-journal-carryover-items "TODO=\"TODO\"|TODO=\"STARTED\"|TODO=\"BLOCKED\"|TODO=\"ASSIGNED\"|TODO=\"SCHEDULED\"")
  (org-journal-enable-agenda-integration t)
  (org-journal-dir "~/ws/gtd/journals/")
  (customize-set-variable 'org-journal-date-format "%A, %x")
  (org-journal-file-format "%Y%m.org")
  (org-journal-file-type 'monthly))

;; org-journal capture
;; Refer to https://github.com/bastibe/org-journal
(defun org-journal-find-location ()
  ;; Open today's journal, but specify a non-nil prefix argument in order to
  ;; inhibit inserting the heading; org-capture will insert the heading.
  (org-journal-new-entry t)
  ;; Position point on the journal's top-level heading so that org-capture
  ;; will add the new entry as a child entry.
  (goto-char (point-min)))

(defun org-journal-delete ()
  ;; Delete current journal file and buffer
  (interactive)
  (delete-file (buffer-name))
  (kill-buffer))
;; }}

;; {{ org-roam
(use-package org-roam
  :ensure t
  :defer t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/ws/gtd/roam"))
  (org-roam-graph-viewer #'eww-open-file)
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         :map org-mode-map
         ("C-M-i" . completion-at-point)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol))
(setq org-roam-node-display-template "${title} ${tags}")
;; }}

;; {{ org-noter
(use-package org-noter
  :ensure t
  :defer t
  :bind (:map org-noter-notes-mode-map
         ("C-x o" . other-window))
  :config
  (pdf-tools-install))
;; }}

;; }} Org Mode Extensions

;; {{ Custom Org Mode Functions
;; My useless functions (can be achieved via much easier yasnippet)
(defun insert-src-in-orgmode (lang)
  "Insert src prefix and postfix for LANG in OrgMode"
  (interactive "sChoose your language: ")
  (newline)
  (indent-for-tab-command)
  (insert "#+begin_src " lang "\n")
  (indent-for-tab-command)
  (save-excursion
    (insert "#+end_src"))
  (org-edit-special)
  )

;; My org template
(defun phye/org-template ()
  (insert "#+title: \n")
  (insert "#+setupfile: ~/.emacs.d/misc/include.org\n"))

(define-auto-insert "\\.org$" #'phye/org-template)

;; see http://thread.gmane.org/gmane.emacs.orgmode/42715
(with-eval-after-load 'org-list
  '(add-hook 'org-checkbox-statistics-hook (function ndk/checkbox-list-complete)))
(defun ndk/checkbox-list-complete ()
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (point)) end)
      (end-of-line)
      (setq end (point))
      (goto-char beg)
      (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
            (if (match-end 1)
                (if (equal (match-string 1) "100%")
                    ;; all done - do the state change
                    (org-todo 'done)
                  (org-todo 'todo))
              (if (and (> (match-end 2) (match-beginning 2))
                       (equal (match-string 2) (match-string 3)))
                  (org-todo 'done)
                (org-todo 'todo)))))))

;; }}

(provide 'phye-init-org)