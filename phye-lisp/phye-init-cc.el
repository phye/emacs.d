;; {{ c
(defun phye/cc-mode-hook ()
  (setq c-basic-offset 4)
  (set-fill-column 80)
  (c-set-offset 'inlambda 0)
  (hide-ifdef-mode)
  (hs-minor-mode)
  ;; (annotate-mode)
  (rainbow-mode -1))
(local-require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c-mode-common-hook 'phye/prog-mode-hook 80)
(add-hook 'c-mode-common-hook 'phye/cc-mode-hook 90)
;; }}

;; {{ cpp
;; (add-hook 'c-mode-common-hook #'lsp-deferred)
(my-ensure 'clang-format)
(add-to-list 'auto-mode-alist '("CMakeLists.txt" . cmake-mode))
(add-to-list 'auto-mode-alist '(".clang-format" . conf-mode))
;; }}

;; {{ Golang
;; (with-eval-after-load 'go-mode
;;   (require 'go-guru))
(use-package go-mode
  :ensure t
  :defer t
  :custom
  (gofmt-command "goimports"))
(defun phye/go-mode-hook ()
    "phye's golang hook"
  (interactive)
  (set-fill-column 80)
  ; (annotate-mode)
  )
(with-eval-after-load 'go-mode
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'lsp-go-install-save-hook)
  (add-hook 'go-mode-hook 'phye/go-mode-hook 90))
(defun lsp-go-install-save-hook ()
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  ;;(add-hook 'before-save-hook 'gofmt-before-save)
  )
;; }}

(provide 'phye-init-cc)