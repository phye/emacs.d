;;; phye-init-misc.el --- Miscellaneous Emacs settings  -*- lexical-binding: t; -*-

;;; Commentary:
;; Miscellaneous settings: fonts, language, display, and macOS tweaks.

;;; Code:

(declare-function exec-path-from-shell-copy-env "exec-path-from-shell")

(defvar zsh-program)
(defvar my-term-program)
(defvar dictionary-server)

;; chinese font
(use-package cnfonts :ensure t :defer t)

(use-package emacs-everywhere :ensure t :defer t)

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(my-run-with-idle-timer
 2
 (lambda ()
   (global-display-line-numbers-mode)
   (setq zsh-program (string-trim (shell-command-to-string "which zsh")))
   (setq my-term-program zsh-program)
   (exec-path-from-shell-copy-env "LOCATION")
   (savehist-mode)
   (when (display-graphic-p)
     (server-start))))

(set-language-environment "utf-8")

(customize-set-variable 'native-comp-async-report-warnings-errors 'silent)
(customize-set-variable 'warning-minimum-level :error)

(setq dictionary-server "dict.org")

(setq help-window-select t)

;; {{ macOS
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

(customize-set-variable 'calendar-latitude +31.2)
(customize-set-variable 'calendar-longitude +121.5)
(customize-set-variable 'ring-bell-function #'ignore)

;; }}

(provide 'phye-init-misc)
;;; phye-init-misc.el ends here
