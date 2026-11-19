;;; phye-init-ai.el --- AI assistant configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; agent-shell and related AI assistant packages.

;;; Code:

(defvar agent-shell-google-gemini-acp-command)

(use-package
 agent-shell
 :ensure t
 :defer t
 :hook (agent-shell-mode . (lambda () (display-line-numbers-mode -1)))
 :config (setq agent-shell-prefer-viewport-interaction t)
 (setq agent-shell-permission-responder-function
       (lambda (permission)
         (when-let* (((equal (map-elt (map-elt permission :tool-call) :kind) "read"))
                     (choice
                      (seq-find
                       (lambda (option) (equal (map-elt option :kind) "allow_once"))
                       (map-elt permission :options))))
           (funcall (map-elt permission :respond) (map-elt choice :option-id))
           t)))
 (if (equal (getenv "LOCATION") "office")
     (progn
       (setq agent-shell-google-gemini-acp-command
             (cons "gemini-internal" (cdr agent-shell-google-gemini-acp-command)))
       (setq agent-shell-anthropic-claude-environment
             (agent-shell-make-environment-variables
              "CLAUDE_CODE_EXECUTABLE"
              (string-trim-right (shell-command-to-string "which claude-internal")))))
   (setq agent-shell-anthropic-authentication
         (agent-shell-anthropic-make-authentication :api-key (getenv "KIMI_AUTH_TOKEN")))
   (setq agent-shell-anthropic-claude-environment
         (agent-shell-make-environment-variables
          "ENABLE_TOOL_SEARCH" "false" "ANTHROPIC_BASE_URL" "https://api.kimi.com/coding/"))))

(defun phye/switch-agent-shell ()
  "Toggle between an agent-shell buffer and a file buffer in the current project.
When the current buffer is in `agent-shell-mode', switch to the most
recently used file buffer in the current project.  Otherwise switch to
the most recent agent-shell buffer in the project, or invoke
`agent-shell' if none exists."
  (interactive)
  (let* ((proj (project-current))
         (proj-buffers (and proj (project-buffers proj)))
         (candidates (seq-filter (lambda (b) (memq b proj-buffers))
                                 (buffer-list)))
         (in-agent-shell (derived-mode-p 'agent-shell-mode))
         (target
          (seq-find
           (lambda (b)
             (and (not (eq b (current-buffer)))
                  (with-current-buffer b
                    (if in-agent-shell
                        (and (buffer-file-name)
                             (not (derived-mode-p 'agent-shell-mode)))
                      (derived-mode-p 'agent-shell-mode)))))
           candidates)))
    (cond
     (target (switch-to-buffer target))
     (in-agent-shell (message "No file buffer in current project"))
     (t (let ((agent-shell-context-sources nil))
          (call-interactively #'agent-shell))))))

(provide 'phye-init-ai)

;;; phye-init-ai.el ends here
