;;; phye-init-ai.el --- AI assistant configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; agent-shell and related AI assistant packages.

;;; Code:

(defvar agent-shell-google-gemini-acp-command)

(use-package
 agent-shell
 ;; use the modified version for codebuddy
 :vc (:url "git@git.woa.com:phye/agent-shell.git" :rev "merge_upstream")
 :config
 (setq agent-shell-google-gemini-acp-command
       (cons "gemini-internal" (cdr agent-shell-google-gemini-acp-command)))
 (setq agent-shell-anthropic-claude-environment
       (agent-shell-make-environment-variables
         "CLAUDE_CODE_EXECUTABLE" (string-trim-right (shell-command-to-string "which claude-internal"))))

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
