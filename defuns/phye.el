;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; General Edit Configs ;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; {{ Misc
(cd "~/ws")
;; although I don't use Diary Mode, change the default file in case of mistyping
(setq diary-file "~/ws/gtd/diary.org")
;; (setq help-window-select t)
(setq vc-follow-symlinks t)
(setq my-disable-wucuo t)
(set-fill-column 100)
(setq my-term-program "/usr/local/bin/zsh")
(set-language-environment "utf-8")
(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; {{ macOS
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
;; }}


;; color and theme settings
(load-theme 'doom-city-lights t)
(setq my-favorite-color-themes
      '(srcery
        atom-dark
        atom-one-dark
        doom-Iosvkem
        doom-acario-dark
        doom-challenger-deep
        doom-dracula
        doom-gruvbox
        doom-molokai
        doom-monokai-classic
        doom-monokai-machine
        doom-monokai-octagon
        doom-monokai-pro
        doom-monokai-ristretto
        doom-monokai-spectrum
        doom-material-dark
        doom-gruvbox
        doom-xcode
        doom-nova
        doom-nord
        doom-nord-aurora
        doom-material-dark
        doom-zenburn
        tango-dark
        solarized-dark-high-contrast
        sanityinc-solarized-dark
        sanityinc-tomorrow-eighties
        sanityinc-tomorrow-night
        modus-vivendi
        spacemacs-dark
        planet
        dakrone
        doom-city-lights
        doom-material
        kaolin-galaxy
        kaolin-bubblegum
        kaolin-temple
        vscode-dark-plus))
(customize-save-variable
 'highlight-symbol-colors
 '("red"                                ;; red
   "white"                              ;; white
   "green"                              ;; green
   "gray"                               ;; gray
   "blue"                               ;; blue
   "yellow"                             ;; yellow
   "magenta"                            ;; red
   "ivory"                              ;; white
   "cyan"                               ;; green
   "thistle"                            ;; gray
   "navy"                               ;; blue
   "gold"                               ;; yellow
   "maroon"                             ;; red
   "lavender"                           ;; white
   "turquoise"                          ;; green
   "slate gray"                         ;; gray
   "slate blue"                         ;; blue
   "moccasin"                           ;; yellow
   "salmon"                             ;; red
   "snow"                               ;; white
   "chartreuse"                         ;; green
   "honeydew"                           ;; gray
   "dodger blue"                        ;; blue
   "khaki"                              ;; yellow
   "violet red"                         ;; red
   "navajo white"                       ;; white
   "light sea green"                    ;; green
   "light slate gray"                   ;; gray
   "deep sky blue"                      ;; blue
   "light coral"                        ;; yellow
   ))
;; customize avy jump colors
(defun recover-avy-lead-face ()
  (interactive)
  (set-face-attribute 'avy-lead-face nil :foreground "red")
  (set-face-attribute 'avy-lead-face nil :background "navy")
  (set-face-attribute 'avy-lead-face-0 nil :foreground "magenta")
  (set-face-attribute 'avy-lead-face-0 nil :background "green"))
(with-eval-after-load 'avy
  (recover-avy-lead-face))
(use-package hl-todo
  :ensure t
  :config
  (add-to-list 'hl-todo-keyword-faces '("DEBUG"  . "blue"))
  (add-to-list 'hl-todo-keyword-faces '("NOTE"  . "blue"))
  (add-to-list 'hl-todo-keyword-faces '("GOTCHA"  . "#FF4500"))
  (add-to-list 'hl-todo-keyword-faces '("Deprecated"  . "white"))
  (add-to-list 'hl-todo-keyword-faces '("STUB"  . "#1E90FF")))

(advice-add 'my-random-favorite-color-theme :after #'recover-avy-lead-face)
(advice-add 'my-random-healthy-color-theme :after #'recover-avy-lead-face)
(advice-add 'my-random-color-themes :after #'recover-avy-lead-face)

;; {{ global keymaps
(define-key global-map (kbd "C-x C-c") 'delete-frame)
(define-key global-map (kbd "C-x M") 'manual-entry)
(define-key global-map (kbd "M-`") 'other-frame)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c t") 'org-mark-ring-goto)

(my-comma-leader-def
  "ls" 'highlight-symbol
  "ol" 'org-open-at-point
  "sl" 'org-store-link
  "il" 'org-insert-link
  "ov" 'jao-toggle-selective-display
  "gt" 'lsp-find-definition
  "gr" 'lsp-find-references
  "dc" 'godoc-at-point
  "xc" 'suspend-frame)

(my-space-leader-def
  "rt" 'my-random-color-theme
  "nn" 'highlight-symbol-next
  "pp" 'highlight-symbol-prev)
;; }}

;; {{ my own util functions
;; Don't pair double quotes
;; https://emacs.stackexchange.com/questions/26225/dont-pair-quotes-in-electric-pair-mode
(defun phye/set-electric-pair-inhibit-predicate()
  "set electric-pair-inhibit-predicate "
  (interactive)
  (setq electric-pair-inhibit-predicate
    (lambda (c)
      (if (or
           (char-equal c ?\{)
           (char-equal c ?\[)
           (char-equal c ?\()
           (char-equal c ?\')
           (char-equal c ?\"))
          t
        (electric-pair-default-inhibit c)))))
(with-eval-after-load 'elec-pair
  (phye/set-electric-pair-inhibit-predicate))

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
  (dired (concat "/sshx:" host ":~/ws")))

;; }}

(require-package 'use-package)

;; chinese font
(use-package cnfonts
  :ensure t
  :defer 5)

;; recentf
(use-package sync-recentf
  :ensure t
  :custom
  (recentf-auto-cleanup 10)
  :config
  (recentf-mode 1))

;; gpg encrypt
(require 'epa-file)
(epa-file-enable)

;; code toggle
;; from: https://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
(defun jao-toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))
;; }}

;; {{ multi project

;; -- projectile-mode
(use-package projectile
  :ensure t
  ;; :bind (("C-c x" . projectile-command-map))
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c x") 'projectile-command-map)
  )

;; per window call stack
(defun my-xref-pop-marker-stack ()
  "Project aware buffer pop"
  (interactive)
  (let ((ring xref--marker-ring)
        (history-buffers (window-prev-buffers)))
    (add-to-list 'history-buffers (list (window-buffer) (point-min) (point)))
    (when (ring-empty-p ring)
      (user-error "Marker stack is empty"))
    (let* ((ring-length (ring-length ring))
          (i 0)
          (found nil))
      ;; xref--marker-ring is a ring, 0 means the newest inserted element, which
      ;; should be popped firstly in this case
      (while (and (< i ring-length) (not found))
        (let* ((marker (ring-ref ring i))
               (buffer (marker-buffer marker))
               (buffer-name (buffer-name buffer)))
          (let ((j 0))
            (while (and (< j (length history-buffers)) (not found))
              (when (eq buffer-name (buffer-name (car (nth j history-buffers))))
                  (setq found t))
              (setq j (1+ j))
              )))
        (setq i (1+ i)))
      (unless found
        (user-error "Marker stack not found"))
      ;; NOTE (phye): i is incred even when found, hence the 1-
      (let ((marker (ring-remove ring (1- i))))
        (switch-to-buffer (or (marker-buffer marker)
                              (user-error "The marker buffer has been deleted")))
        (goto-char (marker-position marker))
        (set-marker marker nil nil)
        (run-hooks 'xref-after-return-hook)))))
(define-key evil-normal-state-map (kbd "C-t") 'my-xref-pop-marker-stack)
;; }}

;; {{ buffer and window related
(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*godoc"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package hide-mode-line
  :ensure nil
  :defer 10)
;; }}

;; {{ evil customizations
(use-package evil-escape
  :ensure t
  :custom
  (evil-escape-key-sequence "fd"))
(use-package evil-numbers
  :ensure t
  :bind (("C-a" . evil-numbers/inc-at-pt)))
;; }}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Programming Related Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; {{ general programming

(with-eval-after-load 'counsel-etags
  (setq counsel-etags-debug t)
  (add-to-list 'counsel-etags-ignore-directories "pack")
  (add-to-list 'counsel-etags-ignore-directories "model")
  (add-to-list 'counsel-etags-ignore-directories "third_path"))

;; company
(defun phye/prog-mode-hook ()
  (turn-on-auto-fill)
  (hs-minor-mode)
  (hl-todo-mode 1)
  (subword-mode)
  (set-fill-column 80)
  (ws-butler-mode -1)  ; disable auto white space removal
  (phye/set-electric-pair-inhibit-predicate))
(add-hook 'prog-mode-hook 'phye/prog-mode-hook 90)

(use-package company
  :custom
  (company-tooltip-limit 20)                      ; bigger popup window
  (company-idle-delay .3)                         ; decrease delay before autocompletion popup shows
  (company-echo-delay 0)                          ; remove annoying blinking
  (company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  :config
  (global-set-key (kbd "<tab>") #'company-indent-or-complete-common))

(with-eval-after-load 'company
  (define-key company-active-map
              (kbd "TAB")
              #'company-complete-common-or-cycle)
  (define-key company-active-map
              (kbd "<backtab>")
              (lambda ()
                (interactive)
                (company-complete-common-or-cycle -1))))

;; camelCase, snake_case .etc
(use-package string-inflection
  :ensure t
  :config
  (define-key global-map (kbd "C-c i") 'string-inflection-cycle)
  (define-key global-map (kbd "C-c C") 'string-inflection-camelcase)
  (define-key global-map (kbd "C-c L") 'string-inflection-lower-camelcase))
;; }}

;; {{ lsp-mode
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "M-l")
  :ensure t
  :hook (
         (go-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :custom
  (lsp-idle-delay 0.500)
  (lsp-enable-symbol-highlighting nil)
  (lsp-go-directory-filters ["-vendor"])
  :config
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]vendor")
  :commands lsp)

;; optionally lsp dependencies
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :after (lsp-mode))
(use-package lsp-ivy
  :ensure t
  :commands lsp-ivy-workspace-symbol
  :after (lsp-mode))
(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list
  :after (lsp-mode))

;; optional if you want which-key integration
;; (use-package which-key
;;     :config
;;     (which-key-mode))
;; }}

;; {{ protobuf
(use-package protobuf-mode
  :ensure t
  :defer 5
  :config
  (add-hook 'protobuf-mode-hook 'phye/prog-mode-hook 90))
;; }}

;; {{ c
(local-require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-basic-offset 2)
            (c-set-offset 'inlambda 0)
            (setq fill-column 90)
            ) t)
;; }}

;; {{ cpp
;; (add-hook 'c-mode-common-hook #'lsp-deferred)
(my-ensure 'clang-format)
;; }}

;; {{ Golang
;; (with-eval-after-load 'go-mode
;;   (require 'go-guru))
(use-package go-mode
  :ensure t
  :defer 5)
(defun phye/golang-hook ()
    "phye's golang hook"
  (interactive)
  (set-fill-column 90)
  (turn-off-auto-fill))
(with-eval-after-load 'go-mode
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'lsp-go-install-save-hook)
  (add-hook 'go-mode-hook 'phye/golang-hook 90))
(defun lsp-go-install-save-hook ()
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  ;;(add-hook 'before-save-hook 'gofmt-before-save)
  )
;; }}

;; {{ JavaScript/JSON
(use-package json-mode
  :ensure t
  :defer 5
  :custom
  (js-indent-level 2)
  (json-encoding-default-indentation "  ")
  :config
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
  (add-hook 'json-mode-hook #'hs-minor-mode))
;; }}

;; {{ YAML
;; from: https://github.com/yoshiki/yaml-mode/issues/25
(use-package yaml-mode
  :ensure t
  :defer 5
  :mode (".yaml$")
  :hook
  (yaml-mode . yaml-mode-outline-hook)

  :init
  (defun yaml-outline-level ()
    "Return the outline level based on the indentation, hardcoded at 2 spaces."
    (s-count-matches "[ ]\\{2\\}" (match-string 0)))

  (defun yaml-mode-outline-hook ()
    (outline-minor-mode t)
    (setq outline-regexp
          (rx
           (seq
            bol
            (group (zero-or-more "  ")
                   (or (group
                        (seq (or (seq "\"" (*? (not (in "\"" "\n"))) "\"")
                                 (seq "'" (*? (not (in "'" "\n"))) "'")
                                 (*? (not (in ":" "\n"))))
                             ":"
                             (?? (seq
                                  (*? " ")
                                  (or (seq "&" (one-or-more nonl))
                                      (seq ">-")
                                      (seq "|"))
			                      eol))))
		               (group (seq
			                   "- "
			                   (+ (not (in ":" "\n")))
			                   ":"
			                   (+ nonl)
			                   eol)))))))
    (setq outline-level 'yaml-outline-level))
  )
;; }}

;; {{ ASM
;; }}

;; {{ Dockerfile
(use-package dockerfile-mode
  :ensure t
  :defer 5
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile_" . dockerfile-mode))
  )
;; }}

;; {{ python
(setq elpy-rpc-python-command (string-trim (shell-command-to-string "which python3")))
;; }}


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Document Edit Configs ;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; {{ markdown
(defun phye/markdown-hook ()
    "diasable trunc lines"
  (interactive)
  (setq truncate-lines nil))
(use-package markdown-mode
  :ensure t
  :defer 5
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
  (add-hook 'markdown-mode-hook 'phye/markdown-hook 90))
(use-package ox-gfm
  :ensure t
  :defer 5)
;; }}

;; {{ latex
(use-package company-math
  :ensure t
  :defer 5
  :config
  (add-to-list 'company-backends 'company-math-symbols-latex)
  (add-to-list 'company-backends 'company-math-symbols-unicode))
;; }}

;; {{ pdf
(use-package pdf-tools
  :ensure t
  :defer 5
  :custom
  (pdf-view-use-scaling t))
;; }}

;; {{ info mode
(defun phye/info-mode-hook ()
  "bind evil like windmov"
  (interactive)
  (global-unset-key (kbd "C-w"))
  (define-key global-map (kbd "C-w h") 'evil-window-left)
  (define-key global-map (kbd "C-w l") 'evil-window-right)
  (define-key global-map (kbd "C-w j") 'evil-window-down)
  (define-key global-map (kbd "C-w k") 'evil-window-up))
(add-hook 'Info-mode-hook #'phye/info-mode-hook 90)
;; }}

;; {{ plantuml
(use-package plantuml-mode
    :ensure t
    :defer 5
    :custom
    (org-plantuml-jar-path "~/.emacs.d/misc/plantuml.jar")
    (plantuml-jar-path "~/.emacs.d/misc/plantuml.jar")
    (plantuml-default-exec-mode 'jar)
    (plantuml-indent-level 0)
    :config
    (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))
;; }}

;; {{ Org Mode

;; {{ OrgMode keybindings
;; Protect my favorite short keys
;; {{ hooks
(defun phye/org-mode-hook ()
  "custom orgmode settings"
  (interactive)
  (setq org-confirm-babel-evaluate nil)
  (set-fill-column 90)
  (setq org-babel-default-header-args:plantuml
        '((:results . "replace")
          (:exports . "results")))
  (turn-on-auto-fill)
  (setq org-tags-column -80)
  (setq org-catch-invisible-edits (quote error))
  (setq safe-local-variable-values (quote ((lentic-init . lentic-orgel-org-init))))
  (setq org-deadline-warning-days 7)
  (setq org-log-into-drawer t)
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (setq org-src-window-setup 'split-window-below)
  (setq org-archive-location "archive.org.gpg::datetree/* From %s")
  (setq org-adapt-indentation t)
  (hl-todo-mode 1)
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
        '((sequence "TODO(t!/!)" "SCHEDULED(S@/@)" "STARTED(s!/!)" "BLOCKED(b@/@)" "|" "DONE(d)" "DEFERED(f@/@)" "CANCELLED(c@/!)") ;; general todo items
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

  ;; Org Mode Capture
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/ws/gtd/gtd.org" "Tasks")
           "* TODO %?\n %i\n %a")
          ("n" "Note" entry (file+datetree "~/ws/gtd/quick_notes.org")
           "* %?\nEntered on %U\n %i\n %a")
          ("j" "Journal entry" entry (function org-journal-find-location)
           "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")))

  (require 'ox-md nil t)
  (require 'ox-odt nil t)

  ;; {{ OrgMode Output
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
          "xelatex -interaction nonstopmode -output-directory %o %f"))

  ;; org babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (ditaa . t)
     (plantuml . t)
     (dot . t)
     ))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (define-key org-mode-map (kbd "C-c o") 'org-open-at-point))
;; }}

;; }}

;; {{ org-mode extensions

;; {{ org-journal related
(use-package org-journal
  :ensure t
  :defer 5
  :custom
  (org-journal-carryover-items "TODO=\"TODO\"|TODO=\"STARTED\"|TODO=\"BLOCKED\"|TODO=\"ASSIGNED\"|TODO=\"SCHEDULED\"")
  (org-journal-enable-agenda-integration t)
  (org-journal-dir "~/ws/gtd/journals/")
  ;; (customize-set-variable 'org-journal-date-format "%A, %Y-%m-%d")
  (org-journal-file-format "%Y%m%d.org")
  (org-journal-file-type 'weekly))

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

;; }}

;; {{ artist
(defun artist-mode-toggle-emacs-state ()
  (if artist-mode
      (evil-emacs-state)
    (evil-exit-emacs-state)))
(add-hook 'artist-mode-hook #'artist-mode-toggle-emacs-state)
;; }}

;; }} Org Mode extensions

;; }} Org Mode


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


;;;;;;;;;;;;;;;
;; ;; End ;; ;;
;;;;;;;;;;;;;;;
