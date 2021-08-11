;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; General Edit Configs ;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; {{ Misc
(cd "~/ws")
;; Donot turn on auto fill for all text mode
(add-hook 'org-mode-hook 'turn-on-auto-fill)
(load-theme 'kaolin-galaxy t)
;; although I don't use Diary Mode, change the default file in case of mistyping
(setq diary-file "~/ws/OrgNotes/diary.org")
(require-package 'cnfonts)
(customize-save-variable
 'highlight-symbol-colors
 '("red"
   "green"
   "yellow"
   "blue"
   "magenta"
   "cyan"
   "dim gray"
   "orange red"
   "dark gray"
   "orange"
   "chartreuse"
   "gray"
   "spring green"
   "deep sky blue"
   "dark violet"
   "light coral"
   "sienna"
   "navajo white"
   "cornsilk"
   "light sea green"
   "light salmon"
   "turquoise"
   "slate blue"
   "slate gray"
   "lavender"
   ))
(setq my-disable-wucuo t)
(setq my-disable-lazyflymake t)


;; Don't pair double quotes
;; https://emacs.stackexchange.com/questions/26225/dont-pair-quotes-in-electric-pair-mode
;; (setq electric-pair-inhibit-predicate
;;       (lambda (c)
;;         (if (char-equal c ?\") t (electric-pair-default-inhibit c))))

(setq help-window-select t)
(setq vc-follow-symlinks t)

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )

;; remote edit
;; devnet
(defun remote-edit (host)
  (interactive "sChoose your host: ")
  (dired (concat "/ssh:" host ":~/ws")))

;; {{ macOS
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
;; }}

;; (require 'chinese-fonts-setup)
;; (run-at-time nil (* 5 60) 'recentf-save-list)
(global-set-key (kbd "C-x C-c") 'delete-frame)
;; (global-set-key (kbd "C-x C-q") 'server-shutdown) prevent server shutdown
;; }}

;; {{ multi project -- emacs-purpose
(require-package 'window-purpose)
(purpose-mode)
(add-to-list 'purpose-user-mode-purposes '(cc-mode . cpp))
(add-to-list 'purpose-user-mode-purposes '(go-mode . golang))
(add-to-list 'purpose-user-mode-purposes '(org-mode . orgmode))
(add-to-list 'purpose-user-mode-purposes '(json-mode . json))
(add-to-list 'purpose-user-mode-purposes '(elisp-mode . elisp))
(purpose-compile-user-configuration)
(my-comma-leader-def
  "xb" 'purpose-switch-buffer-with-purpose
  "xd" 'purpose-toggle-window-purpose-dedicated)
(my-space-leader-def
  "xs" 'purpose-save-window-layout
  "xl" 'purpose-load-window-layout)
;; }}

;; {{ folding
(require-package 'vimish-fold)
(define-key evil-normal-state-map "zf" 'vimish-fold)
(define-key evil-normal-state-map "za" 'vimish-fold-toggle)
(define-key evil-normal-state-map "zA" 'vimish-fold-toggle-all)
(define-key evil-normal-state-map "zd" 'vimish-fold-delete)
(define-key evil-normal-state-map "zD" 'vimish-fold-delete-all)
(define-key evil-normal-state-map "zc" 'vimish-fold-refold)
(define-key evil-normal-state-map "zC" 'vimish-fold-refold-all)
(define-key evil-normal-state-map "zo" 'vimish-fold-unfold)
(define-key evil-normal-state-map "zO" 'vimish-fold-unfold-all)
(define-key evil-normal-state-map "z;" 'vimish-fold-avy)
;; }}

;; {{ buffer and window related
(global-set-key (kbd "C-x M") 'manual-entry)
(set-language-environment "utf-8")
(local-require 'dedicate-windows-manually)
(defun phye/split-windows ()
  (interactive)
  (split-window-right)
  (other-window 1)
  (split-window-right)
  (other-window 1)
  (balance-windows)
  )
;; }}

;; {{ evil customizations
(setq-default evil-escape-key-sequence "jk")
;; }}




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Programming Related Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; {{ general programming
;; company
(setq company-tooltip-limit 20)                      ; bigger popup window
(setq company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
(setq company-echo-delay 0)                          ; remove annoying blinking
(setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing

;; camelCase, snake_case .etc
(require-package 'string-inflection)
(global-set-key (kbd "C-c i") 'string-inflection-cycle)
(global-set-key (kbd "C-c C") 'string-inflection-camelcase)        ;; Force to CamelCase
(global-set-key (kbd "C-c L") 'string-inflection-lower-camelcase)  ;; Force to lowerCamelCase
;; }}

;; {{ lsp-mode
(require-package 'lsp-mode)
(setq lsp-enable-symbol-highlighting nil)
;; }}

;; {{ protobuf
;; (require-package 'protobuf-mode)
;; }}

;; {{ c
(local-require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq fill-column 80)
            (setq c-basic-offset 2)
            (c-set-offset 'inlambda 0)
            ) t)
;; }}

;; {{ cpp
;; (add-hook 'c-mode-common-hook #'lsp-deferred)
(my-ensure 'clang-format)
;; }}

;; {{ Golang
(add-hook 'go-mode-hook #'lsp-deferred)
(with-eval-after-load 'go-mode
  (require 'go-guru))
;; }}

;; {{ JavaScript/JSON
(require-package 'json-mode)
(setq js-indent-level 2)
(setq json-encoding-default-indentation "  ")
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
(add-hook 'json-mode-hook #'hs-minor-mode)
;; }}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Document Edit Configs ;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; {{ markdown
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
(require-package 'ox-gfm)
;; }}

;; {{ latex
(require-package 'company-math)
(require 'company-math)
(add-to-list 'company-backends 'company-math-symbols-latex)
(add-to-list 'company-backends 'company-math-symbols-unicode)
;; }}

;; {{ Org Mode

;; {{ OrgMode keybindings
;; Protect my favorite short keys
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c o") 'org-open-at-point)
(define-key global-map (kbd "C-c t") 'org-mark-ring-goto)

;; My often used org commands
(my-comma-leader-def
       "ol" 'org-open-at-point
       "sl" 'org-store-link
       "il" 'org-insert-link)

(my-space-leader-def
       "rt" 'my-random-color-theme
       "nn" 'highlight-symbol-next
       "pp" 'highlight-symbol-prev)
;; }}

;; {{ hooks
(add-hook 'org-mode-hook
          (lambda ()
            (set-fill-column 100)
            (turn-on-auto-fill)))
;; }}

;; {{ Simple Org settings
(setq org-catch-invisible-edits (quote error))
(setq safe-local-variable-values (quote ((lentic-init . lentic-orgel-org-init))))
(setq org-tags-column -80)
(setq org-deadline-warning-days 7)
(setq org-log-into-drawer t)
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)
(setq org-src-window-setup 'current-window)
;; }}

;; {{ Complex Org settings
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
      '((sequence "TODO(t!/!)" "SCHEDULED(S@/@)" "STARTED(s!/!)" "BLOCKED(b@/@)" "|" "DONE(d)" "DEFERED(f@/@)" "CANCELLED(c@/!)") ;; general todo items
        (sequence "ASSIGNED(a@/!)" "REPRODUCED(p@)" "RCFOUND(r@)" "|" "FIXED(x!)" "VERIFIED(v!)") ;; bug only
        (type "APPT(p)" "REMINDER(m!)" "|" "DONE(d)"))) ;; misc daily items


(setq org-agenda-files
     (quote
      ("~/ws/OrgNotes/gtd.org"
       "~/ws/OrgNotes/quick_notes.org"
       "~/ws/OrgNotes/life/plan_life.org"
       "~/ws/OrgNotes/life/birthday.org"
       "~/ws/OrgNotes/learn/plan_learning.org"
       "~/ws/OrgNotes/work/plan_work.org")))

(setq org-refile-targets
      '((nil :maxlevel . 5)
        (org-agenda-files :maxlevel . 5)
        ("KnowledgeBase.org" :maxlevel . 5)
        ("done.org" :maxlevel . 5)))


;; Org Mode Capture
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/ws/OrgNotes/gtd.org" "Tasks")
         "* TODO %?\n %i\n %a")
        ("n" "Note" entry (file+datetree "~/ws/OrgNotes/quick_notes.org")
         "* %?\nEntered on %U\n %i\n %a")
        ("j" "Journal entry" entry (function org-journal-find-location)
                               "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")))

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (ditaa . t)
   (plantuml . t)
   ))
;; }}

;; {{ OrgMode Output
(setq org-export-with-sub-superscripts nil)
(setq org-export-with-properties t)

(eval-after-load "org"
                 '(require 'ox-md nil t))
(eval-after-load "org"
                 '(require 'ox-odt nil t))

(eval-after-load "ox-latex"
  ;; update the list of LaTeX classes and associated header (encoding, etc.)
  ;; and structure
  '(add-to-list 'org-latex-classes
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

;; ox-taskjuggler
(local-require 'ox-taskjuggler)
(add-to-list 'org-export-backends 'taskjuggler)
(setq org-taskjuggler-target-version 3.6)
;; }}

;; {{ custom orgmode functions
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

;; {{ org-journal related
(customize-set-variable 'org-journal-carryover-items "TODO=\"TODO\"|TODO=\"STARTED\"|TODO=\"BLOCKED\"|TODO=\"ASSIGNED\"|TODO=\"SCHEDULED\"")
(customize-set-variable 'org-journal-enable-agenda-integration t)
(customize-set-variable 'org-journal-dir "~/ws/OrgNotes/journals/")
(customize-set-variable 'org-journal-date-format "%A, %Y-%m-%d")
(customize-set-variable 'org-journal-file-format "%Y%m%d.org")
(customize-set-variable 'org-journal-file-type 'weekly)
(require-package 'org-journal)
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

;; {{ plantuml
(setq plantuml-jar-path "~/.emacs.d/misc/plantuml.jar")
(setq plantuml-default-exec-mode 'jar)
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(add-to-list
  'org-src-lang-modes '("plantuml" . plantuml))
(setq org-plantuml-jar-path
      (expand-file-name "~/.emacs.d/misc/plantuml.jar"))
;; }}

;; {{ artist
(defun artist-mode-toggle-emacs-state ()
  (if artist-mode
      (evil-emacs-state)
    (evil-exit-emacs-state)))
(add-hook 'artist-mode-hook #'artist-mode-toggle-emacs-state)
;; }}


;; }}

;;;;;;;;;;;;;;;
;; ;; End ;; ;;
;;;;;;;;;;;;;;;
