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
(add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-mode))
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

(provide 'phye-init-cc)