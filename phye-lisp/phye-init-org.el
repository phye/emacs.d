;; general editting
(setq org-catch-invisible-edits (quote error))
(setq org-adapt-indentation t)

;; capture, refile and archive
(setq org-refile-targets
      '((nil :maxlevel . 5)
        (org-agenda-files :maxlevel . 5)
        ("KnowledgeBase.org" :maxlevel . 5)
        ("~/ws/gtd/work/draft.org" :maxlevel . 5)
        ("~/ws/gtd/done.org" :maxlevel . 5)))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/ws/gtd/gtd.org" "Tasks")
         "* TODO %?\n %i\n %a")
        ("n" "Note" entry (file+datetree "~/ws/gtd/quick_notes.org")
         "* %?\nEntered on %U\n %i\n %a")
        ("j" "Journal entry" entry (function org-journal-find-location)
         "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")))
(setq org-archive-location "archive.org.gpg::datetree/* From %s")

;; org-babel (src)
(setq org-src-window-setup 'split-window-below)
(setq org-confirm-babel-evaluate nil)
(setq org-babel-default-header-args:plantuml
      '((:results . "file")
        (:exports . "results")))
(setq org-babel-default-header-args:icalendar
      '((:exports . "none")))
(setq org-babel-python-command "python3")


;; gtd status related
(setq org-log-into-drawer t)
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

;; gtd time related
(setq org-deadline-warning-days 7)
(setq org-clock-persist 'history)
(setq org-agenda-files
      (quote
       ("~/ws/gtd/gtd.org"
        "~/ws/gtd/Journelly.org"
        "~/ws/gtd/quick_notes.org"
        "~/ws/gtd/life/plan_life.org"
        "~/ws/gtd/life/birthday.org"
        "~/ws/gtd/learn/plan_learning.org"
        "~/ws/gtd/work/plan_work.org.gpg")))

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
  ;; must be defined here, as org-agenda-mode will reset keybindings
  (general-define-key
   :keymaps 'org-agenda-mode-map
   "W" 'org-agenda-week-view
   "F" 'org-agenda-fortnight-view
   "M" 'org-agenda-month-view
   "Y" 'org-agenda-year-view
   "j" 'org-agenda-next-line
   "k" 'org-agenda-previous-line
   "C" 'org-agenda-capture
   "D" 'org-agenda-convert-date)
  (customize-set-variable 'calendar-chinese-all-holidays-flag t)
  (customize-set-variable 'calendar-holidays
                          (append holiday-oriental-holidays
                                  holiday-local-holidays
                                  holiday-other-holidays)))
(with-eval-after-load 'ox-icalendar
  (customize-set-variable 'org-icalendar-combined-name "OrgMode")
  (customize-set-variable 'org-icalendar-combined-agenda-file "~/ws/gtd/gtd.ics"))

;; output related
(setq org-export-with-sub-superscripts nil)
(setq org-export-with-properties nil)
(setq org-export-with-priority t)

;; output/latex
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
        "xelatex -interaction nonstopmode -output-directory %o %f"))

;; {{ Org Mode Extensions

;; {{ org-journal related
(use-package org-journal
  :ensure t
  :defer t
  :custom
  (org-journal-carryover-items "TODO=\"TODO\"|TODO=\"DOING\"|TODO=\"SCHEDULED\"")
  (org-journal-enable-agenda-integration t)
  (org-journal-dir "~/ws/gtd/journals/")
  (org-journal-file-format "%Y%m.org")
  (org-journal-file-type 'monthly)
  :config
  (customize-set-variable 'org-journal-date-format "%A, %x"))
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

;; {{ org-noter -- pdf annotation
(use-package org-noter
  :ensure t
  :defer t
  :bind (:map org-noter-notes-mode-map
              ("C-x o" . other-window))
  :config
  (pdf-tools-install))
;; }}

;; {{ org-side-tree
(use-package org-side-tree
  :ensure t
  :defer t)
;; }}

;; not in melpa yet https://github.com/rougier/book-mode
;; (use-package book-mode
;;   :ensure t
;;   :defer t)

;; {{ org-remark -- plain text annotation
(defun phye/org-remark-notes-file-name ()
  "Return notes file name as <project>-notes.org."
  (let* ((root (ffip-project-root))
         (prefix (file-name-nondirectory (directory-file-name (file-name-directory root)))))
    (if (string-prefix-p "." prefix)
        (format "%s%s-notes.org" root prefix)
      (format "%s.%s-notes.org" root prefix))))

(use-package org-remark
  :ensure t
  :defer t
  :init
  (org-remark-global-tracking-mode 1)
  :config
  (evil-set-initial-state 'org-remark-mode 'normal)
  :custom
  (org-remark-notes-file-name #'phye/org-remark-notes-file-name)
  (org-remark-notes-display-buffer-action '((display-buffer-below-selected)
                                            (window-height 30)
                                            (preserve-size t))))

(defun phye/mark-and-open (begin end &optional id mode)
  "mark region and open notes"
  (interactive (org-remark-region-or-word))
  (org-remark-mark begin end id mode)
  (org-remark-open (point)))
(defun phye/remark-view-and-select ()
  "review remark and select window"
  (interactive)
  (org-remark-view (point))
  (select-window (get-buffer-window org-remark-notes-buffer-name)))
;; }}

;; {{ calfw
(use-package calfw
  :ensure t
  :defer t)
(use-package calfw-org
  :ensure t
  :defer t)
;; }}

;; {{ org-fancy-priorities
(use-package org-fancy-priorities
  :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :custom
  (org-fancy-priorities-list
   '((?A . "P0")
     (?B . "P1")
     (?C . "P2"))))
;; }}

;; {{ more org babel backends
(use-package ob-mermaid
  :ensure t
  :defer t)
;; }}

;; }} Org Mode Extensions

;; {{ Custom Org Mode Functions
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

(defvar cn-mark-en-rxp
  (rx-to-string
   `(seq
     (group-n 1
       (category chinese))
     (group-n 2
       (any "~+_*"))
     (group-n 3
       (or
        (any alnum)
        (any "-/{}")))))
  "Regexp for match chinese-mark-english.")

(defvar en-mark-cn-rxp
  (rx-to-string
   `(seq
     (group-n 1
       (or
        (any alnum)
        (any "-/{}")))
     (group-n 2
       (any "~+_*"))
     (group-n 3
       (category chinese))))
  "Regexp for matching english-mark-chinese.")

(defun phye/remove-zws-in-region (begin end)
  "Insert zero width whitespace between cn and en characters in BEGIN and END."
  (interactive "r")
  (replace-regexp-in-region "​" "" begin end))

(defun phye/insert-zws-in-region (begin end)
  "Insert zero width whitespace between cn and en characters in BEGIN and END."
  (interactive "r")
  (replace-regexp-in-region cn-mark-en-rxp "\\1​\\2\\3" begin end)
  (replace-regexp-in-region en-mark-cn-rxp "\\1\\2​\\3" begin end))

(defun phye/insert-zws-in-buffer ()
  "Insert zero width whitespace in whole buffer."
  (interactive)
  (phye/insert-zws-in-region (point-min) (point-max)))

(defvar phye/in-word-white-spaces-regex "\\(\\cc\\) +\\(\\cc\\)" "Regexp for in word white spaces.")

(defun phye/cleanup-white-spaces (begin end)
  "Delete white spaces between two Chinese characters in region BEGIN and END."
  (interactive)
  (replace-regexp-in-region
   phye/in-word-white-spaces-regex
   "\\1\\2"
   begin
   end))

(defun phye/cleanup-white-spaces-in-string (str)
  "Cleanup white spaces in STR."
  (replace-regexp-in-string
   phye/in-word-white-spaces-regex
   "\\1\\2"
   str))

(defun phye/cleanup-white-spaces-in-buffer ()
  "Delete white spaces in buffer."
  (interactive)
  (phye/cleanup-white-spaces (point-min) (point-max)))
;; }}


;; {{ hooks
(defun phye/org-mode-hook ()
  "custom orgmode settings"
  (interactive)
  ;; (setq safe-local-variable-values (quote ((lentic-init . lentic-orgel-org-init))))
  (set-fill-column 100))
(add-hook 'org-mode-hook 'phye/org-mode-hook 90)
(add-hook 'org-mode-hook 'phye/prog-mode-hook 80)

(defun phye/org-icalendar-after-save-hook (file)
  "After ical FILE is generated, upload them to my personal website."
  (shell-command (format "scp %s phye-arch:/home/phye/ws/webdav/" file))
  (message "%s uploaded to phye-arch" file))

(add-hook 'org-icalendar-after-save-hook #'phye/org-icalendar-after-save-hook)
;; }}

;; settings after org loaded
(with-eval-after-load 'org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "SCHEDULED(s/@)" "DOING(i)" "|" "DONE(d)" "ONHOLD(h@)" "CANCELLED(c@)") ;; general todo items
          (sequence "DESIGNING(D!)" "CODING(C!)" "TESTING(T!)" "WAITING(W@/!)" "RELEASING(G!)" "|" "RELEASED(R@)") ;; dev todo items
          (type "APPT(p)" "REMINDER(m!)" "|" "DONE(d)"))) ;; misc daily items
  (setq org-tags-column -80)
  (setq org-export-headline-levels 6)
  ;; workaround org-gpg hang issue
  (fset 'epg-wait-for-status 'ignore)
  (setq org-list-allow-alphabetical t)
  (setq-default tab-width 8)
  (setq org-icalendar-include-todo t)
  (setq org-icalendar-use-scheduled
        '(event-if-todo event-if-not-todo todo-start))
  (setq org-icalendar-use-deadline
        '(event-if-todo event-if-not-todo todo-due))
  (setq org-icalendar-alarm-time 60)
  (setq org-icalendar-ttl "PT1H")
  (my-run-with-idle-timer
   1
   (lambda ()
     ;; org babel
     (org-clock-persistence-insinuate)
     (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
     (org-babel-do-load-languages
      'org-babel-load-languages
      '(
        (mermaid .t)
        (ditaa . t)
        (plantuml . t)
        (dot . t)
        (emacs-lisp . t)
        (gnuplot . t)
        ))
     (require 'graphviz-dot-mode)
     (require 'ox-md nil t)
     (require 'ox-odt nil t)
     ;; (require 'calfw-org)
     (require 'yasnippet)
     (my-yas-reload-all))))

(defun phye/org-before-save-hook ()
  "phye's orgmode before save hook."
  (when (member major-mode '(org-mode markdown-mode gfm-mode))
    (when (eq major-mode 'org-mode)
      (phye/insert-zws-in-buffer))
    (phye/cleanup-white-spaces-in-buffer)))
(add-hook 'before-save-hook #'phye/org-before-save-hook)

;; TODO(phye): move this priority logic to md-after-export-hook
(defun phye/replace-priority-in-string (str)
  "Replace #[A|B|C] priority with P[0|1|2] in STR."
  (replace-regexp-in-string
   "#A" "P0"
   (replace-regexp-in-string
    "#B" "P1"
    (replace-regexp-in-string
     "#C" "P2" str))))

(defun phye/md-after-export-hook (text backend info)
  "Cleanup white spaces in TEXT when BACKEND is md, INFO is not used."
  (when (org-export-derived-backend-p backend 'md)
    (phye/replace-priority-in-string
     (phye/cleanup-white-spaces-in-string text))))
(add-hook 'org-export-filter-final-output-functions #'phye/md-after-export-hook)

;; Persistent notes (like persistent-scratch, but built-in)
(customize-set-variable 'remember-data-file "~/ws/gtd/remember.org")
(customize-set-variable 'remember-notes-initial-major-mode 'org-mode)
(customize-set-variable 'remember-notes-auto-save-visited-file-name t)
(customize-set-variable 'remember-in-new-frame t)

(provide 'phye-init-org)