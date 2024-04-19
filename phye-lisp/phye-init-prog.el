(setq zsh-program (string-trim (shell-command-to-string "which zsh")))
(setq my-term-program zsh-program)
(set-language-environment "utf-8")

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
  (add-to-list 'counsel-etags-ignore-filenames "*.pb.cc"))

(with-eval-after-load 'find-file-in-project
  (add-to-list 'ffip-prune-patterns "*/build")
  (add-to-list 'ffip-prune-patterns "*/rpm_build")
  (add-to-list 'ffip-prune-patterns "*/cc_tool")
  (add-to-list 'ffip-prune-patterns "*/qci_files")
  (add-to-list 'ffip-prune-patterns "*/vendor")
  (add-to-list 'ffip-ignore-filenames "*.pb.cc")
  (add-to-list 'ffip-ignore-filenames "*.pb.h")
  (add-to-list 'ffip-ignore-filenames "*_pb2.py"))

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

;; python
(setq elpy-rpc-python-command (string-trim (shell-command-to-string "which python3")))

(use-package tree-sitter
  :ensure t
  :defer t)

(use-package tree-sitter-langs
  :ensure t
  :defer t
  :after (tree-sitter))

;; (setq treesit-extra-load-path (list (format "%s/elpa/tree-sitter-langs-0.12.150/bin" user-emacs-directory)))

(defun phye/goto-definition-at-point ()
  "my mode-aware go to definition"
  (interactive)
  (cl-case major-mode
    (go-mode (xref-find-definitions (symbol-at-point)))
    (python-mode (elpy-goto-definition))
    (t (counsel-etags-find-tag-at-point))))

(with-eval-after-load 'eldoc-mode
  (setq eldoc-idle-delay 5))

(evil-set-initial-state 'godoc-mode 'normal)
(add-hook 'go-mode-hook 'eglot-ensure)

;; use magit to edit commit message
(require 'git-commit)

;; general prog-mode-hook
(defun phye/prog-mode-hook ()
  (interactive)
  "phye's prog mode hook"
  (turn-on-auto-fill)
  (hs-minor-mode)
  (hl-todo-mode 1)
  (subword-mode)
  (ws-butler-mode -1)                   ; disable auto white space removal
  ;; (phye/set-electric-pair-inhibit-predicate)
  (setq my-disable-lazyflymake t)
  (set-fill-column 100))
(add-hook 'prog-mode-hook 'phye/prog-mode-hook 90)

(defun phye/python-mode-hook ()
  "phye's python mode hook"
  (pyvenv-activate "~/ws/pyvenv"))
(add-hook 'python-mode-hook 'phye/python-mode-hook 90)

(provide 'phye-init-prog)