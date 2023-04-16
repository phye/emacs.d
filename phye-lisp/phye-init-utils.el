;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (save-some-buffers)
  (recentf-save-list)
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
  (with-temp-buffer
    (insert ts)
    (mark-whole-buffer)
    (copy-to-x-clipboard))
  (message "ts: %s" ts))

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

(provide 'phye-init-utils)