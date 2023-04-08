(defun phye/markdown-hook ()
    "diasable trunc lines"
  (interactive)
  (setq truncate-lines nil))

(use-package markdown-mode
  :ensure t
  :defer t
  :config
  (add-hook 'markdown-mode-hook 'phye/markdown-hook 90))

;; org mode export github style markdown
(use-package ox-gfm
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

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

(provide 'phye-init-doc)