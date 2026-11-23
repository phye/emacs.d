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

(defvar-local phye--agent-shell-return-marker nil
  "Marker to the position to return to when leaving an `agent-shell' buffer.")

(defun phye--agent-shell-side-p (&optional buffer)
  "Return non-nil if BUFFER (or current) is an agent-shell or viewport buffer."
  (with-current-buffer (or buffer (current-buffer))
    (derived-mode-p 'agent-shell-mode
                    'agent-shell-viewport-edit-mode
                    'agent-shell-viewport-view-mode)))

(defun phye/switch-agent-shell ()
  "Toggle between an agent-shell buffer and a file buffer in the current project.
When in a file buffer, remember the current position and switch to the
most recent agent-shell buffer in the project, or invoke `agent-shell'
if none exists.  When in an `agent-shell-mode' or viewport buffer, jump
back to the remembered position, falling back to the most recently used
file buffer in the project.  Viewport buffers are treated as part of
the agent-shell side, so this command never toggles between the main
agent-shell buffer and its viewport."
  (interactive)
  (let* ((proj (project-current))
         (proj-buffers (and proj (project-buffers proj)))
         (candidates (seq-filter (lambda (b) (memq b proj-buffers))
                                 (buffer-list)))
         (in-agent-shell (phye--agent-shell-side-p)))
    (if in-agent-shell
        (let ((marker phye--agent-shell-return-marker))
          (if (and (markerp marker) (marker-buffer marker))
              (progn
                (switch-to-buffer (marker-buffer marker))
                (goto-char marker))
            (if-let ((target (seq-find
                              (lambda (b)
                                (and (not (eq b (current-buffer)))
                                     (with-current-buffer b
                                       (and (buffer-file-name)
                                            (not (phye--agent-shell-side-p b))))))
                              candidates)))
                (switch-to-buffer target)
              (message "No file buffer in current project"))))
      (let ((origin-marker (point-marker))
            (target (seq-find
                     (lambda (b)
                       (and (not (eq b (current-buffer)))
                            (with-current-buffer b
                              (derived-mode-p 'agent-shell-mode))))
                     candidates)))
        (if target
            (switch-to-buffer target)
          (let ((agent-shell-context-sources nil))
            (call-interactively #'agent-shell)))
        (when (derived-mode-p 'agent-shell-mode)
          (setq phye--agent-shell-return-marker origin-marker))))))

(provide 'phye-init-ai)

;;; phye-init-ai.el ends here
