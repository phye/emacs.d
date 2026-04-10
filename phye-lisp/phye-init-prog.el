;;; phye-init-prog.el --- General programming configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Shell pop, tree-sitter, eglot, xref, and prog-mode hooks.

;;; Code:

;; quick pop shell
(use-package
 shell-pop
 :ensure t
 :defer t
 :config
 (setq shell-pop-shell-type
       '("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell))))
 (setq shell-pop-term-shell zsh-program)
 ;; need to do this manually or not picked up by `shell-pop'
 (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

(use-package multi-vterm :ensure t :defer t)

(customize-set-variable 'sh-basic-offset 2)

;; code annotation
(use-package
 annotate
 :ensure t
 :defer t
 :custom
 (annotate-summary-ask-query t)
 (annotate-file "~/.emacs.data.d/annotations"))

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
      '("build" "duiqi" "data" "pack" "cache"
        ;; "model"
        "lib" "third_path"))

;; counsel
(with-eval-after-load 'counsel-etags
  (setq counsel-etags-debug t)
  (setq counsel-etags-ignore-directories
        (append phye/general-ignore-directories counsel-etags-ignore-directories))
  (add-to-list 'counsel-etags-ignore-filenames "*_pb2.py")
  (add-to-list 'counsel-etags-ignore-filenames "*.pb.h")
  (add-to-list 'counsel-etags-ignore-filenames "*.pb.cc")
  (add-to-list 'counsel-etags-ignore-filenames "*.pb.go")
  (add-to-list 'counsel-etags-ignore-filenames "*.org"))

(with-eval-after-load 'find-file-in-project
  ;; (add-to-list 'ffip-prune-patterns "*/build")
  (add-to-list 'ffip-prune-patterns "*/rpm_build")
  (add-to-list 'ffip-prune-patterns "*/cc_tool")
  (add-to-list 'ffip-prune-patterns "*/qci_files")
  (add-to-list 'ffip-prune-patterns "*/vendor")
  (add-to-list 'ffip-ignore-filenames "*.pb.cc")
  (add-to-list 'ffip-ignore-filenames "*.pb.h")
  (add-to-list 'ffip-ignore-filenames "*_pb2.py")
  (add-to-list 'ffip-ignore-filenames "*.pb.go"))

;; ediff
(defvar ediff-previous-theme nil
  "Previous theme before ediff for backup.")
(defun phye/ediff-startup-hook ()
  "Save current theme and load doom-gruvbox for ediff."
  (setq ediff-previous-theme (car custom-enabled-themes))
  (load-theme 'doom-gruvbox t))
(defun phye/ediff-cleanup-hook ()
  "Restore previous theme and run ediff janitor."
  (load-theme ediff-previous-theme t)
  (ediff-janitor nil t))
(add-hook 'ediff-startup-hook #'phye/ediff-startup-hook)
(add-hook 'ediff-cleanup-hook #'phye/ediff-cleanup-hook)

;; log
(defun phye/view-log-with-color ()
  "Apply ANSI color codes in the current buffer."
  (interactive)
  (ansi-color-apply-on-region (point-min) (point-max)))

;; tree-sitter
(use-package tree-sitter :ensure t :defer t :config (setq treesit-font-lock-level 4))

(use-package ts-fold :load-path "~/.emacs.d/site-lisp/ts-fold" :custom (ts-fold-line-count-show t))

(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(with-eval-after-load 'ts-fold
  (push '(block_sequence_item . ts-fold-range-seq) (alist-get 'yaml-mode ts-fold-range-alist))
  (push '(try_statement . ts-fold-range-seq) (alist-get 'python-mode ts-fold-range-alist))
  (push '(if_statement . ts-fold-range-seq) (alist-get 'python-mode ts-fold-range-alist)))

(use-package ts-fold-indicators :load-path "~/.emacs.d/site-lisp/ts-fold/")

(use-package symbol-overlay :ensure t :config (setq symbol-overlay-inhibit-map t))

(setq-default eglot-workspace-configuration
              '(:pylsp
                (:plugins
                 (:pycodestyle
                  (:enabled t :ignore ["W503" "E203"] :maxLineLength 100)
                  :black (:enabled t :cache-config t)
                  :isort (:enabled t)))))

;; elisp-autofmt
(use-package
 elisp-autofmt
 :ensure t
 :defer t
 :commands (elisp-autofmt-mode elisp-autofmt-buffer)
 :hook (emacs-lisp-mode . elisp-autofmt-mode)
 :config
 (if *is-a-mac*
     (customize-set-variable 'elisp-autofmt-python-bin "/opt/homebrew/bin/python3")
   (customize-set-variable 'elisp-autofmt-python-bin "/usr/bin/python3")))

;; python
(with-eval-after-load 'eldoc-mode
  (setq eldoc-idle-delay 5))

(defun phye/python-mode-hook ()
  "Phye's python mode hook."
  ;; pip install python-lsp-server
  (customize-set-variable 'python-interpreter "~/.pyvenv/bin/python")
  (pyvenv-activate "~/.pyvenv")
  (setq tab-width 4)
  (ts-fold-mode t)
  (eglot-ensure))

(add-hook 'python-mode-hook 'phye/python-mode-hook 0)

(defun phye/goto-definition-at-point ()
  "My mode-aware go to definition."
  (interactive)
  (cl-case
   major-mode
   (go-mode (xref-find-definitions (symbol-at-point)))
   (python-mode (xref-find-definitions (symbol-at-point)))
   (org-mode (org-open-at-point))
   (typescript-mode (xref-find-definitions (symbol-at-point)))
   (typescript-ts-mode (xref-find-definitions (symbol-at-point)))
   (emacs-lisp-mode (my-evil-goto-definition))
   (t (counsel-etags-find-tag-at-point))))

(defun phye/format-buffer ()
  "My format buffer wrapper."
  (interactive)
  (cl-case
   major-mode
   (go-mode (gofmt))
   (mermaid-mode t)
   (org-mode t)
   (python-mode (eglot-format-buffer))
   (emacs-lisp-mode (elisp-autofmt-buffer))
   (t (save-excursion (evil-indent (point-min) (point-max))))))

(defun phye/go-back-to-caller ()
  "My mode-aware go back to caller."
  (interactive)
  (cl-case major-mode (org-mode (org-mark-ring-goto)) (t (xref-pop-marker-stack))))

(defun phye/xref-clear-marker-stack ()
  "Interactively clear marker stack."
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
  "Phye's prog mode hook."
  (interactive)
  (turn-on-auto-fill)
  (hs-minor-mode)
  (hl-todo-mode 1)
  (subword-mode)
  (ws-butler-mode -1) ; disable auto white space removal
  (setq my-disable-wucuo t)
  (setq my-disable-lazyflymake t)
  (set-fill-column 100)
  (symbol-overlay-mode))
(add-hook 'prog-mode-hook 'phye/prog-mode-hook 90)

(defun phye/get-project-name-of-active-window ()
  "Return project name of active window."
  (let* ((project-root (ffip-project-root))
         (project-name
          (directory-file-name
           (file-relative-name project-root (file-name-parent-directory project-root))))
         (len (length project-name))
         (shortname
          (if (>= len 10)
              (substring project-name 0 10)
            project-name)))
    shortname))

(defun phye/set-tmux-window-based-on-project (&optional window)
  "Set tmux window name to current active project, WINDOW is not used yet."
  (interactive)
  (when (and (not (display-graphic-p)) (not (string-empty-p (getenv "TMUX"))))
    (let ((name (phye/get-project-name-of-active-window)))
      (shell-command (format "tmux rename-window %s" name)))))

;; (add-to-list 'window-selection-change-functions #'phye/set-tmux-window-based-on-project)

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(provide 'phye-init-prog)
;;; phye-init-prog.el ends here
