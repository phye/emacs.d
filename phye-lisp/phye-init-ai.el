;;; package --- Summary

;;; Commentary:

;;; Code:
(use-package
 agent-shell
 ;; use the modified version for codebuddy
 :vc (:url "git@git.woa.com:phye/agent-shell.git" :rev :newest)
 ;; :ensure t
 ;; to use codebuddy, normally you should only login once and then relies on
 ;; codebuddy's login state to keep session
 )

;; (use-package
;;  agent-shell
;;  :ensure t
;;  :config (setq agent-shell-google-authentication (agent-shell-google-make-authentication :none t))
;;  (setq agent-shell-google-gemini-command
;;        (cons "gemini-internal" (cdr agent-shell-google-gemini-command))))
(provide 'phye-init-ai)

;;; phye-init-ai.el ends here
