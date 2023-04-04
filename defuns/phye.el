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
(setq split-height-threshold nil)
(require-package 'use-package)

;; {{ macOS
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
;; }}

;; color and theme settings
(load-theme 'spacemacs-dark t)
(setq my-favorite-color-themes
      '(srcery
        atom-dark
        atom-one-dark
        doom-dark+
        doom-Iosvkem
        doom-acario-dark
        doom-challenger-deep
        doom-dracula
        doom-gruvbox
        doom-ir-black
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
        doom-oceanic-next
        doom-old-hope
        doom-opera
        doom-zenburn
        doom-palenight
        doom-spacegrey
        tango-dark
        ;; solarized-dark-high-contrast
        ;; sanityinc-solarized-dark
        sanityinc-tomorrow-eighties
        sanityinc-tomorrow-night
        modus-vivendi
        spacemacs-dark
        seti
        planet
        dakrone
        doom-city-lights
        doom-material
        kaolin-galaxy
        kaolin-bubblegum
        kaolin-temple
        cyberpunk
        ;; vscode-dark-plus
        ))
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
  (require 'avy)
  (set-face-attribute 'avy-lead-face nil :foreground "red")
  (set-face-attribute 'avy-lead-face nil :background "navy")
  (set-face-attribute 'avy-lead-face-0 nil :foreground "magenta")
  (set-face-attribute 'avy-lead-face-0 nil :background "green"))
(with-eval-after-load 'avy
  (recover-avy-lead-face))
(use-package hl-todo
  :ensure t
  :custom
  (hl-todo-keyword-faces
   '(("STUB" . "#1E90FF")
     ("Deprecated" . "white")
     ("PITFALL" . "#FF4500")
     ("LOGIC" . "yellow")
     ("PURPOSE" . "lavender")
     ("THOUGHT" . "orange")
     ("DEBUG" . "blue")
     ("HOLD" . "#d0bf8f")
     ("TODO" . "#cc9393")
     ("NEXT" . "#dca3a3")
     ("THEM" . "#dc8cc3")
     ("PROG" . "#7cb8bb")
     ("OKAY" . "#7cb8bb")
     ("DONT" . "#5f7f5f")
     ("FAIL" . "#8c5353")
     ("DONE" . "#afd8af")
     ("NOTE" . "#d0bf8f")
     ("KLUDGE" . "#d0bf8f")
     ("HACK" . "#d0bf8f")
     ("TEMP" . "#d0bf8f")
     ("FIXME" . "#cc9393")
     ("XXX+" . "#cc9393"))))

(advice-add 'my-random-favorite-color-theme :after #'recover-avy-lead-face)
(advice-add 'my-random-healthy-color-theme :after #'recover-avy-lead-face)
(advice-add 'my-random-color-themes :after #'recover-avy-lead-face)
(defun phye/clean-symbol (&optional symbol)
  (interactive)
  "Filter symbol without leading $,@ characters"
  (let* ((symbol (or symbol
                     (symbol-at-point)
                    (error "No symbol at point")))
         (symbolstr (symbol-name symbol))
         (c (elt symbolstr 0)))
    (if (or (= c ?$)
            (= c ?@))
        (list (seq-subseq symbolstr 1)) ;; return symbol without first char
      (list symbolstr))))
;; (advice-add 'highlight-symbol :filter-args #'phye/clean-symbol)

;; {{ global keymaps
(define-key global-map (kbd "C-x C-c") 'delete-frame)
(define-key global-map (kbd "C-x M") 'manual-entry)
(define-key global-map (kbd "M-`") 'other-frame)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c t") 'org-mark-ring-goto)
(define-key global-map (kbd "M-v") 'paste-from-x-clipboard)

(my-comma-leader-def
  "ls" 'highlight-symbol
  "ol" 'org-open-at-point
  "sl" 'org-store-link
  "il" 'org-insert-link
  "id" 'find-file-in-current-directory
  "ov" 'jao-toggle-selective-display
  "gt" 'lsp-find-definition
  "gr" 'lsp-find-references
  "id" 'find-file-in-current-directory
  "dc" 'godoc-at-point
  "xb" 'ivy-switch-buffer
  "xc" 'suspend-frame
  "cc" 'clipetty-kill-ring-save)

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
  ;; (phye/set-electric-pair-inhibit-predicate)
  )

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

;; chinese font
(use-package cnfonts
  :ensure t
  :defer t)

;; recentf
(use-package sync-recentf
   :ensure t
   :custom
   (recentf-auto-cleanup 60)
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

;; ediff
(setq previous-theme nil)
(defun phye/ediff-startup-hook ()
  (setq previous-theme (car custom-enabled-themes))
  (load-theme 'doom-dracula t))
(defun phye/ediff-cleanup-hook ()
    (load-theme previous-theme t)
    (winner-undo))
(add-hook 'ediff-startup-hook #'phye/ediff-startup-hook)
(add-hook 'ediff-cleanup-hook #'phye/ediff-cleanup-hook)

;; clipboard
(use-package clipetty
  :ensure t
  :custom
  (clipetty-tmux-ssh-tty "tmux show-environment SSH_TTY"))
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
(define-key evil-motion-state-map (kbd "C-x C-l") 'winner-undo)
(define-key evil-motion-state-map (kbd "C-x C-r") 'winner-redo)
;; }}

;; {{ buffer and window related
(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle-latest)
         ("s-`"   . popper-cycle)
         ("C-s-`" . popper-toggle-type))
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
  :ensure t
  :defer t)

(define-key minibuffer-local-map (kbd "C-a") 'move-beginning-of-line)
(define-key minibuffer-local-map (kbd "C-e") 'move-end-of-line)
(define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)
(with-eval-after-load 'ivy-mode
  (define-key ivy-minibuffer-map (kbd "C-w") 'evil-delete-backward-word))

(defun dedicate-current-window ()
  "Dedicate current window"
  (interactive)
  (set-window-dedicated-p (selected-window) t)
  (message "window dedicated"))
(defun undedicate-current-window ()
  "Undedicate current window"
  (interactive)
  (set-window-dedicated-p (selected-window) nil)
  (message "window undedicated"))
;; }}

;; file and dirs
;;preview files in dired
(use-package peep-dired
  :ensure t
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

;; {{ evil customizations
(use-package evil-escape
  :ensure t
  :custom
  (evil-escape-delay 0.2)
  (evil-escape-key-sequence "fd"))
(use-package evil-numbers
  :ensure t
  :bind (:map evil-normal-state-map ("C-a" . evil-numbers/inc-at-pt)))
;; }}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; Programming Related Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; {{ git
;; copied from https://github.com/abo-abo/hydra/wiki/Version-Control
(defhydra aboabo/hydra-git-gutter (:body-pre (git-gutter-mode 1)
                            :hint nil)
  "
Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk        set start _R_evision
"
  ("j" git-gutter:next-hunk)
  ("k" git-gutter:previous-hunk)
  ("h" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("l" (progn (goto-char (point-min))
              (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("p" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("q" nil :color blue)
  ("Q" (progn (git-gutter-mode -1)
              ;; git-gutter-fringe doesn't seem to
              ;; clear the markup right away
              (sit-for 0.1)
              (git-gutter:clear))
       :color blue))
(global-set-key (kbd "C-x C-g") 'aboabo/hydra-git-gutter/body)
;; }}

;; {{ general programming
(use-package shell-pop
  :ensure t
  :defer t
  :bind (("M-t" . shell-pop))
  :config
  (setq shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell))))
  (setq shell-pop-term-shell "/bin/zsh")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(with-eval-after-load 'counsel-etags
  (setq counsel-etags-debug t)
  (add-to-list 'counsel-etags-ignore-directories "duiqi")
  (add-to-list 'counsel-etags-ignore-directories "data")
  (add-to-list 'counsel-etags-ignore-directories "cache")
  (add-to-list 'counsel-etags-ignore-directories "pack")
  (add-to-list 'counsel-etags-ignore-directories "model")
  (add-to-list 'counsel-etags-ignore-directories "lib")
  (add-to-list 'counsel-etags-ignore-directories "build")
  (add-to-list 'counsel-etags-ignore-directories "third_path")
  (add-to-list 'counsel-etags-ignore-directories "netcapture/proto")
  (add-to-list 'counsel-etags-ignore-directories "crm_client/dm_nlp_svrs/nlp_structured_msg_svr/client/proto")
  (add-to-list 'counsel-etags-ignore-filenames "*_pb2.py")
  (add-to-list 'counsel-etags-ignore-filenames "*.pb.h")
  (add-to-list 'counsel-etags-ignore-filenames "*.pb2.cc"))

(use-package annotate
  :ensure t
  :custom
  (annotate-summary-ask-query t))

(defun phye/prog-mode-hook ()
  (turn-on-auto-fill)
  (hs-minor-mode)
  (hl-todo-mode 1)
  (subword-mode)
  (set-fill-column 100)
  (ws-butler-mode -1)  ; disable auto white space removal
  ;; (phye/set-electric-pair-inhibit-predicate)
  )
(add-hook 'prog-mode-hook 'phye/prog-mode-hook 90)

(defun phye/view-log-with-color ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; company
(use-package company
  :custom
  (company-echo-delay 0)                          ; remove annoying blinking
  (company-begin-commands '(self-insert-command))
  :config
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-backends (delete 'company-clang company-backends))
  ) ; start autocompletion only after typin)

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
  (lsp-verify-signature nil)
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
  :defer t
  :config
  (add-hook 'protobuf-mode-hook 'phye/prog-mode-hook 90))
;; }}

;; {{ c
(local-require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c-mode-common-hook
          (lambda ()
            (setq c-basic-offset 4)
            (c-set-offset 'inlambda 0)
            (setq fill-column 90)
            (rainbow-mode -1)
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
  :defer t
  :custom
  (gofmt-command "goimports"))
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

;; {{ binary
(evil-set-initial-state 'hexl-mode 'emacs)
(defun phye/hexl-mode-hook ()
  "phye's hexl mode hook"
    (interactive)
    (read-only-mode t))
(use-package hexl
  :config
  (add-to-list 'auto-mode-alist '("\\.bin\\'" . hexl-mode))
  (add-to-list 'auto-mode-alist '("\\.data\\'" . hexl-mode))
  (define-key hexl-mode-map (kbd "n") 'hexl-forward-char)
  (define-key hexl-mode-map (kbd "e") 'hexl-forward-short)
  (define-key hexl-mode-map (kbd "E") 'hexl-forward-word)
  (define-key hexl-mode-map (kbd "p") 'hexl-backward-char)
  (define-key hexl-mode-map (kbd "b") 'hexl-backward-short)
  (define-key hexl-mode-map (kbd "B") 'hexl-backward-word)
  :hook
  (hexl-mode . phye/hexl-mode-hook))
;; }}

;; {{
;; conf
(add-to-list 'auto-mode-alist '("\\.txt\\'" . conf-mode))
;; }}

;; {{ JavaScript/JSON
(use-package json-mode
  :ensure t
  :defer t
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
  :defer t
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
  :defer t
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
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))
  (add-hook 'markdown-mode-hook 'phye/markdown-hook 90))
(use-package ox-gfm
  :ensure t
  :defer t)
;; }}

;; {{ latex
(use-package company-math
  :ensure t
  :defer t
  :config
  ;; (add-to-list 'company-backends 'company-math-symbols-latex)
  ;; (add-to-list 'company-backends 'company-math-symbols-unicode)
  )
;; }}

;; {{ pdf
(use-package pdf-tools
  :ensure t
  :defer t
  :bind (:map pdf-view-mode-map
         (";" . ace-pinyin-jump-char-2)
         ("C-x o" . other-window))
  :config
  (pdf-tools-install)
  (blink-cursor-mode nil)
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  :custom
  (pdf-view-use-scaling t)
  (pdf-annot-activate-created-annotations t)
  (pdf-view-display-size 'fit-page)
  (pdf-view-resize-factor 1.1))
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
    :defer t
    :custom
    (org-plantuml-jar-path "~/.emacs.d/misc/plantuml.jar")
    (plantuml-jar-path "~/.emacs.d/misc/plantuml.jar")
    (plantuml-default-exec-mode 'jar)
    (plantuml-indent-level 0)
    :config
    (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))
;; }}

;; {{ graphviz-dot-mode
(use-package graphviz-dot-mode
  :ensure t
  :defer t)
(add-hook 'graphviz-dot-mode-hook #'turn-off-auto-fill)
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

;; calendar and org-agenda
(customize-set-variable 'holiday-local-holidays
                        '(append
                          (holiday-fixed 1 1 "元旦")
                          (holiday-fixed 3 8 "妇女节")
                          (holiday-fixed 5 1 "劳动节")
                          (holiday-fixed 5 4 "青年节")
                          (holiday-fixed 6 1 "儿童节")
                          (holiday-fixed 7 1 "建党节")
                          (holiday-fixed 8 1 "建军节")
                          (holiday-fixed 10 1 "国庆节")
                          ))
;; org-agenda-mode-map
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

;; {{ org-noter
(use-package org-noter
  :ensure t
  :defer t
  :bind (:map org-noter-notes-mode-map
         ("C-x o" . other-window)))
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
