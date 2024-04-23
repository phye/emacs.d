;; chinese font
(use-package cnfonts
  :ensure t
  :defer t)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(my-run-with-idle-timer
 2
 (lambda ()
   (setq zsh-program (string-trim (shell-command-to-string "which zsh")))
   (setq my-term-program zsh-program)))

(set-language-environment "utf-8")

(customize-set-variable 'native-comp-async-report-warnings-errors 'silent)

(setq dictionary-server "dict.org")

;; {{ macOS
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)
(setq help-window-select t)
;; }}

(provide 'phye-init-misc)