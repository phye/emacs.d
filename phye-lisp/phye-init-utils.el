;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (recentf-save-list)
  (bookmark-save)
  (kill-emacs)
  )

;; remote-edit
(defun remote-edit (host)
  (interactive "sChoose your host: ")
  (dired (concat "/sshx:" host ":~/ws")))

;; timestamp
(defun ts-to-human (ts)
  "Given ts INTEGER, return human readable string"
  (interactive "nts: ")
  (message (format-time-string "%Y-%m-%d %H:%M:%S" ts)))
(defun current-ts ()
  (interactive)
  (setq ts (format "%s" (time-convert (current-time) 'integer)))
  (copy-variable-to-clipboard ts))

;; hex to ascii, copied from stackoverflow
(defun decode-hex-string (hex-string)
  (apply #'concat
         (cl-loop for i from 0 to (- (/ (length hex-string) 2) 1)
                  for hex-byte = (substring hex-string (* 2 i) (* 2 (+ i 1)))
                  collect (format "%c" (string-to-number hex-byte 16)))))

;; code toggle
;; from: https://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
(defun jao-toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))

(defun dos2unix ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'unix 't)
  (save-buffer))

(defun find-file-in-cpp-module ()
  (interactive)
  (find-file-in-current-directory 1)
  )

(defun copy-variable-to-clipboard (val)
  (with-temp-buffer
    (insert val)
    (mark-whole-buffer)
    (clipetty-kill-ring-save))
  (message "copied: %s" val))

(defun copy-relative-dir-in-project ()
  (interactive)
  (setq path (file-name-directory
              (file-relative-name
               (buffer-file-name)
               (ffip-project-root))))
  (copy-variable-to-clipboard path)
  (clipetty-kill-ring-save))

(defun copy-relative-path-in-project ()
  (interactive)
  (setq path (file-relative-name
              (buffer-file-name)
              (ffip-project-root)))
  (copy-variable-to-clipboard path)
  (clipetty-kill-ring-save))

;; My useless functions (can be achieved via much easier yasnippet)
(defun insert-src-in-orgmode (lang)
  "Insert src prefix and postfix for LANG in OrgMode"
  (interactive "sChoose your language: ")
  (newline)
  (indent-for-tab-command)
  (insert "#+begin_src " lang "\n")
  (indent-for-tab-command)
  (save-excursion
    (insert "#+end_src"))
  (org-edit-special)
  )

(defun insert-zero-width-space ()
  "Insert zero width space before and after OrgMode mark."
  (interactive)
  (insert-char (char-from-name "ZERO WIDTH SPACE")))

(provide 'phye-init-utils)
