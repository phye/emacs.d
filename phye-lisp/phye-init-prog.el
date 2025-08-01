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
  :defer t
  :config
  (setq treesit-font-lock-level 4))

(use-package tree-sitter-langs
  :vc (:url "https://github.com/emacs-tree-sitter/tree-sitter-langs.git"
            :branch "0.12.279")
  :defer t
  :after (tree-sitter))

(use-package ts-fold
  :load-path "~/.emacs.d/site-lisp/ts-fold"
  :custom
  (ts-fold-line-count-show t))

(with-eval-after-load 'ts-fold
  (push '(block_sequence_item . ts-fold-range-seq) (alist-get 'yaml-mode ts-fold-range-alist))
  (push '(try_statement . ts-fold-range-seq) (alist-get 'python-mode ts-fold-range-alist))
  (push '(if_statement . ts-fold-range-seq) (alist-get 'python-mode ts-fold-range-alist)))

(use-package ts-fold-indicators
  :load-path "~/.emacs.d/site-lisp/ts-fold/")

;; (setq treesit-extra-load-path (list (format "%s/elpa/tree-sitter-langs-0.12.150/bin" user-emacs-directory)))

(use-package symbol-overlay
  :ensure t
  :config
  (setq symbol-overlay-inhibit-map t))

(setq-default eglot-workspace-configuration
              '(:pylsp
                (:plugins (
                           :pycodestyle (:enabled t :ignore ["W503" "E203"] :maxLineLength 100)
                           :black (:enabled t :cache-config t)
                           :isort (:enabled t)))))

;; python
(with-eval-after-load 'eldoc-mode
  (setq eldoc-idle-delay 5))

(defun phye/python-mode-hook ()
  "phye's python mode hook"
  ;; pip install python-lsp-server
  (customize-set-variable 'python-interpreter "~/.pyvenv/bin/python")
  (pyvenv-activate "~/.pyvenv")
  (setq tab-width 4)
  (ts-fold-mode t)
  (eglot-ensure))

(add-hook 'python-mode-hook 'phye/python-mode-hook 0)

(defun phye/goto-definition-at-point ()
  "my mode-aware go to definition"
  (interactive)
  (cl-case major-mode
    (go-mode (xref-find-definitions (symbol-at-point)))
    (python-mode (xref-find-definitions (symbol-at-point)))
    (org-mode (org-open-at-point))
    (emacs-lisp-mode (evil-goto-definition))
    (t (counsel-etags-find-tag-at-point))))

(defun phye/format-buffer ()
  "My format buffer wrapper."
  (interactive)
  (cl-case major-mode
    (go-mode (gofmt))
    (python-mode (eglot-format-buffer))
    (t (save-excursion
        (evil-indent (point-min) (point-max))))))

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

(defun phye/xref-jump-to-only-reference (IDENTIFIER)
  "Automatically jump to IDENTIFIER if there's only one ref."
  (let ((xref-buffer (get-buffer "*xref*")))
    (with-current-buffer xref-buffer
      ;; if there're only one reference (file followed by reference)
      ;; automatically goto it
      (when (eq (count-lines (point-min) (point-max)) 2)
        (xref-next-line)
        (xref-goto-xref)
        (kill-buffer xref-buffer)))))
(advice-add 'xref-find-references :after #'phye/xref-jump-to-only-reference)

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
  (set-fill-column 100)
  (symbol-overlay-mode))
(add-hook 'prog-mode-hook 'phye/prog-mode-hook 90)

(defun phye/get-project-name-of-active-window ()
  "Return project name of active window."
  (let* ((project-root (ffip-project-root))
         (project-name
          (directory-file-name
           (file-relative-name
            project-root
            (file-name-parent-directory project-root))))
         (len (length project-name))
         (shortname
          (if (>= len 10)
              (substring project-name 0 10)
            project-name)))
    shortname))

(defun phye/set-tmux-window-based-on-project (&optional window)
  "Set tmux window name to current active project, WINDOW is not used yet."
  (interactive)
  (when (and (not (display-graphic-p))
             (not (string-empty-p (getenv "TMUX"))))
    (let ((name (phye/get-project-name-of-active-window)))
      (shell-command (format "tmux rename-window %s" name) t t))))

(add-to-list 'window-selection-change-functions #'phye/set-tmux-window-based-on-project)

(provide 'phye-init-prog)