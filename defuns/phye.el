(setq org-agenda-files
     (quote
      ("~/ws/OrgNotes/gtd.org"
       "~/ws/OrgNotes/LearningPlan.org"
       "~/ws/OrgNotes/KnowledgeBase.org"
       "~/ws/OrgNotes/Work@Cisco.org")))
  (setq org-catch-invisible-edits (quote error))
(setq safe-local-variable-values (quote ((lentic-init . lentic-orgel-org-init))))
(setq c-basic-offset 4)
(setq c-default-style "linux")
(setq fill-column 80)

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

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(cd "~/ws/OrgNotes/")

(setq-default evil-escape-key-sequence "jk")

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/ws/OrgNotes/gtd.org" "Tasks")
         "* TODO %?\n %i\n %a")
        ("n" "Note" entry (file+datetree "~/ws/OrgNotes/quick_notes.org")
         "* %?\nEntered on %U\n %i\n %a")))
(define-key global-map "\C-cc" 'org-capture)

(setq org-refile-targets
      '((nil :maxlevel . 5)
        (org-agenda-files :maxlevel . 5)
        ("KnowledgeBase.org" :maxlevel . 5)
        ("done.org" :maxlevel . 5)
        ("Work@Cisco.org" :maxlevel . 5)))

(eval-after-load "org"
                 '(require 'ox-odt nil t))

(setq org-export-with-sub-superscripts nil)

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq org-export-with-properties t)

(eval-after-load "org"
                 '(require 'ox-md nil t))

(setq org-latex-to-pdf-process
      '("xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"
        "xelatex -interaction nonstopmode -output-directory %o %f"))

;;(require 'chinese-fonts-setup)


(defun auto-complete-for-go ()
  (auto-complete-mode 1))
(add-hook 'go-mode-hook 'auto-complete-for-go)

(with-eval-after-load 'go-mode
   (require 'go-autocomplete)
   (require 'go-guru))
