(setq my-term-program "/usr/local/bin/zsh")
(set-language-environment "utf-8")

;; quick pop shell
(use-package shell-pop
  :ensure t
  :defer t
  :config
  (setq shell-pop-shell-type '("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell))))
  (setq shell-pop-term-shell "/bin/zsh")
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type)
  (my-comma-leader-def
    "tt" 'shell-pop))

;; code annotation
(use-package annotate
  :ensure t
  :defer t
  :custom
  (annotate-summary-ask-query t)
  (annotate-file "~/.data/annotations"))

;; company
(use-package company
  :custom
  (company-echo-delay 0)                          ; remove annoying blinking
  (company-begin-commands '(self-insert-command))
  :config
  (setq company-backends (delete 'company-semantic company-backends))
  (setq company-backends (delete 'company-clang company-backends))
  ) ; start autocompletion only after typin)

;; counsel
(with-eval-after-load 'counsel-etags
  (setq counsel-etags-debug t)
  (add-to-list 'counsel-etags-ignore-directories "duiqi")
  (add-to-list 'counsel-etags-ignore-directories "cc_tool")
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

;; ediff
(defvar previous-theme nil "previous theme before ediff for backup")
(defun phye/ediff-startup-hook ()
  (setq previous-theme (car custom-enabled-themes))
  (load-theme 'doom-dracula t))
(defun phye/ediff-cleanup-hook ()
    (load-theme previous-theme t)
    (winner-undo))
(add-hook 'ediff-startup-hook #'phye/ediff-startup-hook)
(add-hook 'ediff-cleanup-hook #'phye/ediff-cleanup-hook)

;; projectile-mode, multiple projects
(use-package projectile
  :ensure t
  :defer t
  ;; :bind (("C-c x" . projectile-command-map))
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c x") 'projectile-command-map)
  )

;; log
(defun phye/view-log-with-color ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; python
(setq elpy-rpc-python-command (string-trim (shell-command-to-string "which python3")))

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
;; }}


;; general prog-mode-hook
(defun phye/prog-mode-hook ()
  "phye's prog mode hook"
  (turn-on-auto-fill)
  (hs-minor-mode)
  (hl-todo-mode 1)
  (subword-mode)
  (set-fill-column 100)
  (ws-butler-mode -1)                   ; disable auto white space removal
  (annotate-mode)
  ;; (phye/set-electric-pair-inhibit-predicate)
  )
(add-hook 'prog-mode-hook 'phye/prog-mode-hook 90)

(provide 'phye-init-prog)