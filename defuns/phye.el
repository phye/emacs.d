;; {{ Misc
(cd "~/ws/OrgNotes/")
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(load-theme 'darkburn t)
;; although I don't use Diary Mode, change the default file in case of mistyping
(setq diary-file "~/ws/OrgNotes/diary.org")

;; }}

;; buffer related {{
(global-set-key (kbd "C-x M") 'manual-entry)
;; }}


;; {{ macOS
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
;; }}


;; {{ evil customizations
(setq-default evil-escape-key-sequence "jk")
;; }}


;; {{ c programming
(setq c-basic-offset 4)
(setq c-default-style "linux")
(setq fill-column 80)
;; }}


;; {{ Golang programming
(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

(with-eval-after-load 'go-mode
   (require 'go-autocomplete)
   (require 'go-guru))

;; }}

;; {{ calfw
(require-package 'calfw)
(require-package 'calfw-org)
(require-package 'calfw-ical)
(require 'calfw)
(require 'calfw-org)
(require 'calfw-ical)

(defun my-open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source "Green")
    (cfw:ical-create-source "chn-holidays" "https://calendar.google.com/calendar/ical/en.china%23holiday%40group.v.calendar.google.com/public/basic.ics" "Yellow")
    (cfw:ical-create-source "gtd" "https://calendar.google.com/calendar/ical/semimiracle%40gmail.com/private-a04b71b8d901ec5c2abb2cf8f4397ec0/basic.ics" "Orange")
    (cfw:ical-create-source "birthday" "https://calendar.google.com/calendar/htmlembed?src=%23contacts%40group.v.calendar.google.com&ctz=Asia%2FShanghai" "Red")
    )))
;; }}

;; {{ latex
(require-package 'company-math)
(require 'company-math)
(add-to-list 'company-backends 'company-math-symbols-latex)
(add-to-list 'company-backends 'company-math-symbols-unicode)
;; }}

;; {{ Org Mode
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
      '((sequence "TODO(t)" "STARTED(s!)" "WAITING(w@)" "BLOCKED(b@)" "|" "DONE(d!)" "CANCELLED(c@/!)") ;; general todo items
        (sequence "ASSIGNED(a!)" "STARTED(s!)" "REPRODUCED(p@)" "RCFOUND(r@)" "|" "FIXED(f!)" "VERIFIED(v!)") ;; bug only
        (type "APPT(a)" "REMINDER(r!)" "|" "DONE(d)"))) ;; misc daily items

(setq org-agenda-files
     (quote
      ("~/ws/OrgNotes/gtd.org"
       "~/ws/OrgNotes/PlanLearning.org"
       "~/ws/OrgNotes/PlanLife.org"
       "~/ws/OrgNotes/life/Birthday.org"
       "~/ws/OrgNotes/Work.org")))

(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(setq org-refile-targets
      '((nil :maxlevel . 5)
        (org-agenda-files :maxlevel . 5)
        ("KnowledgeBase.org" :maxlevel . 5)
        ("done.org" :maxlevel . 5)))


;; Protect my favorite short keys
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)

;; My often used org commands
(nvmap :prefix ","
       "ol" 'org-open-at-point
       "sl" 'org-store-link
       "il" 'org-insert-link)

(setq org-src-window-setup 'current-window)

;; Org Mode Capture
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/ws/OrgNotes/gtd.org" "Tasks")
         "* TODO %?\n %i\n %a")
        ("n" "Note" entry (file+datetree "~/ws/OrgNotes/quick_notes.org")
         "* %?\nEntered on %U\n %i\n %a")
        ("j" "Journal entry" entry (function org-journal-find-location)
                               "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")))


;; OrgMode Output
(eval-after-load "org"
                 '(require 'ox-md nil t))
(eval-after-load "org"
                 '(require 'ox-odt nil t))
(setq org-export-with-sub-superscripts nil)
(setq org-export-with-properties t)
(setq org-latex-to-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))

;;(require 'chinese-fonts-setup)


;; org-journal related
(setq org-journal-dir "~/ws/OrgNotes/journals/")
(setq org-journal-enable-agenda-integration t)
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

;; Misc Org settings
(setq org-catch-invisible-edits (quote error))
(setq safe-local-variable-values (quote ((lentic-init . lentic-orgel-org-init))))
(setq org-tags-column -79)
(setq org-deadline-warning-days 7)


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
(eval-after-load 'org-list
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
