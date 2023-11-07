;; chinese font
(use-package cnfonts
  :ensure t
  :defer t)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

;; {{ macOS
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
;; }}

(provide 'phye-init-misc)