;; {{ JavaScript/JSON
(use-package
 json-mode
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
(defun phye/yaml-mode-hook ()
  "My yaml mode hook."
  (ts-fold-mode t))

(use-package
 yaml-mode
 :ensure t
 :defer t
 :mode (".yaml$")
 :hook
 (yaml-mode . display-line-numbers-mode)
 (yaml-mode . phye/yaml-mode-hook))
;; }}

;; {{ protobuf
(use-package
 protobuf-mode
 :ensure t
 :defer t
 :config (add-hook 'protobuf-mode-hook 'phye/prog-mode-hook 90))
;; }}

;; conf
(add-to-list 'auto-mode-alist '("\\.txt\\'" . conf-mode))

;; log
(add-to-list 'auto-mode-alist '("\\.log\\'" . log-view-mode))

;; Dockerfile
(use-package
 dockerfile-mode
 :ensure t
 :defer t
 :config (add-to-list 'auto-mode-alist '("Dockerfile_" . dockerfile-mode)))

(provide 'phye-init-data)
