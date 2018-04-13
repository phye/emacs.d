;; {{ Misc
(cd "~/ws/OrgNotes/")
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(load-theme 'darkburn t)
;; although I don't use Diary Mode, change the default file in case of mistyping
(setq diary-file "~/ws/OrgNotes/diary.org")
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


;; {{ Org Mode
(setq org-agenda-files
     (quote
      ("~/ws/OrgNotes/gtd.org"
       "~/ws/OrgNotes/LearningPlan.org"
       "~/ws/OrgNotes/KnowledgeBase.org"
       "~/ws/OrgNotes/Work@Cisco.org")))

(setq org-refile-targets
      '((nil :maxlevel . 5)
        (org-agenda-files :maxlevel . 5)
        ("KnowledgeBase.org" :maxlevel . 5)
        ("done.org" :maxlevel . 5)
        ("Work@Cisco.org" :maxlevel . 5)))


;; Protect my favorite short keys
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)

;; Org Mode Capture
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/ws/OrgNotes/gtd.org" "Tasks")
         "* TODO %?\n %i\n %a")
        ("n" "Note" entry (file+datetree "~/ws/OrgNotes/quick_notes.org")
         "* %?\nEntered on %U\n %i\n %a")))


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
(setq org-journal-dir "~/ws/OrgNotes/journals")
(setq org-journal-enable-agenda-integration t)


;; Misc Org settings
(setq org-catch-invisible-edits (quote error))
(setq safe-local-variable-values (quote ((lentic-init . lentic-orgel-org-init))))


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

;; }}