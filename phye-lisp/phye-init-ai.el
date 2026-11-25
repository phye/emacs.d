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
  "In an `agent-shell' buffer: marker to the file position to return to.")

(defvar-local phye--file-agent-shell-marker nil
  "In a file buffer: marker to the agent-shell position to return to.")

(defun phye--agent-shell-side-p (&optional buffer)
  "Return non-nil if BUFFER (or current) is an agent-shell or viewport buffer."
  (with-current-buffer (or buffer (current-buffer))
    (derived-mode-p 'agent-shell-mode
                    'agent-shell-viewport-edit-mode
                    'agent-shell-viewport-view-mode)))

(defun phye/switch-agent-shell ()
  "Toggle between an agent-shell buffer and a file buffer in the current project.
When in a file buffer, remember the current position and switch to the
most recent agent-shell buffer in the project, restoring the last known
position there, or invoke `agent-shell' if none exists.  When in an
`agent-shell-mode' or viewport buffer, record the current position and
jump back to the remembered file position, falling back to the most
recently used file buffer in the project.  Viewport buffers are treated
as part of the agent-shell side, so this command never toggles between
the main agent-shell buffer and its viewport."
  (interactive)
  (let* ((proj (project-current))
         (proj-buffers (and proj (project-buffers proj)))
         (candidates (seq-filter (lambda (b) (memq b proj-buffers))
                                 (buffer-list)))
         (in-agent-shell (phye--agent-shell-side-p))
         (origin-buffer (current-buffer))
         (origin-marker (point-marker)))
    (if in-agent-shell
        (let ((file-marker phye--agent-shell-return-marker)
              (target (or (and (markerp phye--agent-shell-return-marker)
                               (marker-buffer phye--agent-shell-return-marker))
                          (seq-find
                           (lambda (b)
                             (and (not (eq b (current-buffer)))
                                  (with-current-buffer b
                                    (and (buffer-file-name)
                                         (not (phye--agent-shell-side-p b))))))
                           candidates))))
          (if target
              (progn
                (switch-to-buffer target)
                (when (and (markerp file-marker)
                           (eq (marker-buffer file-marker) target))
                  (goto-char file-marker))
                (when (with-current-buffer origin-buffer
                        (derived-mode-p 'agent-shell-mode))
                  (setq phye--file-agent-shell-marker origin-marker)))
            (message "No file buffer in current project")))
      (let ((saved-agent-marker phye--file-agent-shell-marker)
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
          (setq phye--agent-shell-return-marker origin-marker)
          (when (and (markerp saved-agent-marker)
                     (eq (marker-buffer saved-agent-marker) (current-buffer)))
            (goto-char saved-agent-marker)))))))

(provide 'phye-init-ai)

;;; phye-init-ai.el ends here
