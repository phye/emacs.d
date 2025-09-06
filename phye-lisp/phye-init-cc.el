;; {{ c
(defun phye/cc-mode-hook ()
  (setq c-basic-offset 2)
  (set-fill-column 80)
  (c-set-offset 'inlambda 0)
  (hide-ifdef-mode)
  (hs-minor-mode)
  (annotate-mode)
  (unless (eq major-mode 'protobuf-mode)
    (tree-sitter-hl-mode))
  (rainbow-mode -1)
  (my-ensure 'clang-format))
(local-require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c-mode-common-hook 'phye/prog-mode-hook 80)
(add-hook 'c-mode-common-hook 'phye/cc-mode-hook 90)
;; }}

;; {{ cpp
(my-run-with-idle-timer
 5
 (lambda ()
   (add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
   (add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-mode))
   (add-to-list 'auto-mode-alist '(".clang-format" . conf-mode))))
;; }}

;; {{ Golang
;; (with-eval-after-load 'go-mode
;;   (require 'go-guru))
(evil-set-initial-state 'godoc-mode 'normal)
(add-hook 'go-mode-hook 'eglot-ensure)

(use-package go-mode :ensure t :defer t :custom (gofmt-command "goimports"))

;; eglot-organize-imports is hopefully a temporary stopgap until
;; https://github.com/joaotavora/eglot/issues/574 is addressed.
(defun eglot-organize-imports ()
  "Offer to execute the source.organizeImports code action."
  (interactive)
  (unless (eglot--server-capable :codeActionProvider)
    (eglot--error "Server can't execute code actions!"))
  (let* ((server (eglot--current-server-or-lose))
         (actions
          (jsonrpc-request
           server
           :textDocument/codeAction (list :textDocument (eglot--TextDocumentIdentifier))))
         (action
          (cl-find-if
           (jsonrpc-lambda
            (&key kind &allow-other-keys) (string-equal kind "source.organizeImports"))
           actions)))
    (when action
      (eglot--dcase
       action
       (((Command) command arguments) (eglot-execute-command server (intern command) arguments))
       (((CodeAction) edit command)
        (when edit
          (eglot--apply-workspace-edit edit))
        (when command
          (eglot--dbind
           ((Command) command arguments)
           command
           (eglot-execute-command server (intern command) arguments))))))))

(defun phye/go-mode-hook ()
  "phye's golang hook"
  (interactive)
  (auto-fill-mode -1)
  (tree-sitter-hl-mode)
  (setq compile-command "go test")
  ;; (annotate-mode)
  (ts-fold-mode))
(with-eval-after-load 'go-mode
  (add-hook 'go-mode-hook 'phye/go-mode-hook 90))
;; }}

(provide 'phye-init-cc)
