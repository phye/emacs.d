;; {{ latex
(use-package company-math
  :ensure t
  :defer t
  :config
  ;; (add-to-list 'company-backends 'company-math-symbols-latex)
  ;; (add-to-list 'company-backends 'company-math-symbols-unicode)
  )
;; }}

;; {{ pdf
(use-package pdf-tools
  :ensure t
  :defer t
  :bind (:map pdf-view-mode-map
         (";" . ace-pinyin-jump-char-2)
         ("C-x o" . other-window))
  :config
  (pdf-tools-install)
  (blink-cursor-mode nil)
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  :custom
  (pdf-view-use-scaling t)
  (pdf-annot-activate-created-annotations t)
  (pdf-view-display-size 'fit-page)
  (pdf-view-resize-factor 1.1))
;; }}

(provide 'phye-init-pdf)