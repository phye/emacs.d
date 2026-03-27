;;; phye-init-ai.el --- AI assistant configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; agent-shell and related AI assistant packages.

;;; Code:

(defvar agent-shell-google-gemini-acp-command)

(use-package
  agent-shell
  :ensure t
  ;; use the modified version for codebuddy
  ;; :vc (:url "git@git.woa.com:phye/agent-shell.git" :rev "merge_upstream")
  :config
  (setq agent-shell-prefer-viewport-interaction nil)
  (setq agent-shell-permission-responder-function
        (lambda (permission)
          (when-let (((equal (map-elt (map-elt permission :tool-call) :kind) "read"))
                     (choice
                      (seq-find
                       (lambda (option) (equal (map-elt option :kind) "allow_once"))
                       (map-elt permission :options))))
            (funcall (map-elt permission :respond) (map-elt choice :option-id))
            t)))
  (if (eq (getenv "LOCATION") "office")
      (progn
        (setq agent-shell-google-gemini-acp-command
              (cons "gemini-internal" (cdr agent-shell-google-gemini-acp-command)))
        (setq agent-shell-anthropic-claude-environment
              (agent-shell-make-environment-variables
               "CLAUDE_CODE_EXECUTABLE" (string-trim-right (shell-command-to-string "which claude-internal"))
               )))
    (setq agent-shell-anthropic-authentication
          (agent-shell-anthropic-make-authentication
           :api-key (getenv "KIMI_AUTH_TOKEN")))
    (setq agent-shell-anthropic-claude-environment
          (agent-shell-make-environment-variables
           "ENABLE_TOOL_SEARCH" "false"
           "ANTHROPIC_BASE_URL" "https://api.kimi.com/coding/"))))

(provide 'phye-init-ai)

;;; phye-init-ai.el ends here
