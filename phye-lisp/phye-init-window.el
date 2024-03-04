;; {{ buffer and window related
(setq split-height-threshold nil)
(setq frame-resize-pixelwise t)
(when (boundp 'fringe-mode)
  (fringe-mode 0))

(defun phye/switch-to-previous-buffer-in-window ()
  "Switch to previous buffer in the same window."
  (interactive)
  (let* ((cur-buffer (current-buffer))
        (alt-buffer (car (car (window-prev-buffers)))))
    (if (eq cur-buffer alt-buffer)
        (switch-to-buffer (car (car (cdr (window-prev-buffers)))))
    (switch-to-buffer alt-buffer))))

(use-package popper
  :ensure t
  :defer t
  :init
  :custom
  (popper-reference-buffers
   '(
     "Output\\*$"
     ;; "\\*godoc"
     ;; "\\*Compile-Log\\*$"
     "\\*Async Shell Command\\*"
     "\\*Messages\\*"
     "^\\*ansi-term.*\\*$"
     "^\\*helpful.*\\*$"
     help-mode
     compilation-mode))
  ;; (popper-group-function #'popper-group-by-directory)
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package hide-mode-line
  :ensure t
  :defer t)

(defun dedicate-current-window ()
  "Dedicate current window"
  (interactive)
  (set-window-dedicated-p (selected-window) t)
  (message "window dedicated"))
(defun undedicate-current-window ()
  "Undedicate current window"
  (interactive)
  (set-window-dedicated-p (selected-window) nil)
  (message "window undedicated"))

(setq xref-history-storage 'xref-window-local-history)
;; }}

;; per window call stack
(defun my-xref-pop-marker-stack ()
  "Project aware buffer pop"
  (interactive)
  (let ((ring xref--marker-ring)
        (history-buffers (window-prev-buffers)))
    (add-to-list 'history-buffers (list (window-buffer) (point-min) (point)))
    (when (ring-empty-p ring)
      (user-error "Marker stack is empty"))
    (let* ((ring-length (ring-length ring))
          (i 0)
          (found nil))
      ;; xref--marker-ring is a ring, 0 means the newest inserted element, which
      ;; should be popped firstly in this case
      (while (and (< i ring-length) (not found))
        (let* ((marker (ring-ref ring i))
               (buffer (marker-buffer marker))
               (buffer-name (buffer-name buffer)))
          (let ((j 0))
            (while (and (< j (length history-buffers)) (not found))
              (when (eq buffer-name (buffer-name (car (nth j history-buffers))))
                  (setq found t))
              (setq j (1+ j))
              )))
        (setq i (1+ i)))
      (unless found
        (user-error "Marker stack not found"))
      ;; NOTE (phye): i is incred even when found, hence the 1-
      (let ((marker (ring-remove ring (1- i))))
        (switch-to-buffer (or (marker-buffer marker)
                              (user-error "The marker buffer has been deleted")))
        (goto-char (marker-position marker))
        (set-marker marker nil nil)
        (run-hooks 'xref-after-return-hook)))))
(define-key evil-normal-state-map (kbd "C-t") 'xref-pop-marker-stack)

(defun phye/select-next-frame ()
  "Select next frame and raise it."
  (interactive)
  (select-frame (next-frame)))

(defun phye/select-previous-frame ()
  "Select previous frame and raise it."
  (interactive)
  (select-frame (previous-frame)))

(defun phye/current-frame-name (&optional num)
  "Return current frame name"
  (interactive "P")
  (let ((name (substring-no-properties
               (cdr (assoc 'name (frame-parameters))))))
    (when num
        (message name))
    name))

(defvar phye/last-frame-name (phye/current-frame-name))
(defvar phye/frame-name-before-switch (phye/current-frame-name))
(defvar phye/frame-name-after-switch (phye/current-frame-name))
(defun phye/buffer-list-update-hook ()
  "Update phye/last-frame-name on buffer change."
  (unless (string= (phye/current-frame-name) phye/frame-name-before-switch)
    (setq phye/frame-name-after-switch (phye/current-frame-name))
    (setq phye/last-frame-name phye/frame-name-before-switch)
    (setq phye/frame-name-before-switch phye/frame-name-after-switch)
    (unless (display-graphic-p)
      (message "Switch frame from %s to %s" phye/last-frame-name phye/frame-name-after-switch))))
(add-hook 'buffer-list-update-hook #'phye/buffer-list-update-hook)

(defun phye/toggle-last-frame ()
  "Toggle between last used frame"
  (interactive)
  (select-frame-by-name phye/last-frame-name))

(defun phye/kill-buffer-and-frame ()
  "Kill buffer and frame"
  (interactive)
  (kill-buffer)
  (delete-frame))

(defun phye/open-recent-file-in-other-frame ()
  "Open recent file in other frame."
  (interactive)
  (let* ((files (mapcar #'substring-no-properties recentf-list))
         (hint "Recent files: ")
         (f (ivy-read hint files)))
    (find-file-other-frame f)))

(provide 'phye-init-window)