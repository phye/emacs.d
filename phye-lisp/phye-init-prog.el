;; quick pop shell
(use-package shell-pop
  :ensure t
  :defer t
  :config
  (setq shell-pop-shell-type
        '("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell))))
  (setq shell-pop-term-shell zsh-program)
  ;; need to do this manually or not picked up by `shell-pop'
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(customize-set-variable 'sh-basic-offset 2)

;; code annotation
(use-package annotate
  :ensure t
  :defer t
  :custom
  (annotate-summary-ask-query t)
  (annotate-file "~/.data/annotations"))

;; company
;; (with-eval-after-load 'company-ispell
;;   (setq company-ispell-available nil))
;; (use-package company
;;   :custom
;;   (company-echo-delay 0)                          ; remove annoying blinking
;;   :config
;;   (setq company-backends (delete 'company-semantic company-backends))
;;   (setq company-backends (delete 'company-clang company-backends))
;;   (cons 'company-capf company-backends)
;;   (setq company-backends (cl-remove-duplicates company-backends))) ; start autocompletion only after typin)

(setq phye/general-ignore-directories
  '(
    "build"
    "duiqi"
    "data"
    "pack"
    "cache"
    ;; "model"
    "lib"
    "third_path"))

;; counsel
(with-eval-after-load 'counsel-etags
  (setq counsel-etags-debug t)
  (setq counsel-etags-ignore-directories
        (append phye/general-ignore-directories counsel-etags-ignore-directories))
  (add-to-list 'counsel-etags-ignore-filenames "*_pb2.py")
  (add-to-list 'counsel-etags-ignore-filenames "*.pb.h")
  (add-to-list 'counsel-etags-ignore-filenames "*.pb.cc")
  (add-to-list 'counsel-etags-ignore-filenames "*.pb.go"))

(with-eval-after-load 'find-file-in-project
  (add-to-list 'ffip-prune-patterns "*/build")
  (add-to-list 'ffip-prune-patterns "*/rpm_build")
  (add-to-list 'ffip-prune-patterns "*/cc_tool")
  (add-to-list 'ffip-prune-patterns "*/qci_files")
  (add-to-list 'ffip-prune-patterns "*/vendor")
  (add-to-list 'ffip-ignore-filenames "*.pb.cc")
  (add-to-list 'ffip-ignore-filenames "*.pb.h")
  (add-to-list 'ffip-ignore-filenames "*_pb2.py")
  (add-to-list 'ffip-ignore-filenames "*.pb.go"))

;; ediff
(defvar previous-theme nil "previous theme before ediff for backup")
(defun phye/ediff-startup-hook ()
  (setq previous-theme (car custom-enabled-themes))
  (load-theme 'doom-gruvbox t))
(defun phye/ediff-cleanup-hook ()
  (load-theme previous-theme t)
  (ediff-janitor nil t))
(add-hook 'ediff-startup-hook #'phye/ediff-startup-hook)
(add-hook 'ediff-cleanup-hook #'phye/ediff-cleanup-hook)

;; log
(defun phye/view-log-with-color ()
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

(use-package tree-sitter
  :ensure t
  :defer t)

(use-package tree-sitter-langs
  :ensure t
  :defer t
  :after (tree-sitter))

;; (setq treesit-extra-load-path (list (format "%s/elpa/tree-sitter-langs-0.12.150/bin" user-emacs-directory)))

;; python
(with-eval-after-load 'eldoc-mode
  (setq eldoc-idle-delay 5))
(defun phye/python-mode-hook ()
  "phye's python mode hook"
  (customize-set-variable 'elpy-rpc-python-command "~/ws/pyvenv/bin/python")
  (customize-set-variable 'python-interpreter "~/ws/pyvenv/bin/python")
  (pyvenv-activate "~/ws/pyvenv"))

(add-hook 'python-mode-hook 'phye/python-mode-hook 0)

(defun phye/goto-definition-at-point ()
  "my mode-aware go to definition"
  (interactive)
  (cl-case major-mode
    (go-mode (xref-find-definitions (symbol-at-point)))
    (python-mode (elpy-goto-definition))
    (org-mode (org-open-at-point))
    (t (counsel-etags-find-tag-at-point))))

(defun phye/go-back-to-caller ()
  "My mode-aware go back to caller."
  (interactive)
  (cl-case major-mode
    (org-mode (org-mark-ring-goto))
    (t (xref-pop-marker-stack))))

(defun phye/xref-clear-marker-stack ()
  "Interactively clear marker stack"
  (interactive)
  (xref-clear-marker-stack)
  (message "xref stack cleared"))

;; general prog-mode-hook
(defun phye/prog-mode-hook ()
  (interactive)
  "phye's prog mode hook"
  (turn-on-auto-fill)
  (hs-minor-mode)
  (hl-todo-mode 1)
  (subword-mode)
  (ws-butler-mode -1)                   ; disable auto white space removal
  (setq my-disable-wucuo t)
  (set-fill-column 100))
(add-hook 'prog-mode-hook 'phye/prog-mode-hook 90)

(provide 'phye-init-prog)