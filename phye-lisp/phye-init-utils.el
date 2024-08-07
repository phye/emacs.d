;; define function to shutdown emacs server instance

(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (recentf-save-list)
  (bookmark-save)
  (kill-emacs)
  )

;; remote-edit
(defun remote-edit (addr)
  "Connect to remote server ADDR and eit there."
  (interactive "sChoose your host and port, separated by: ")
  (let* ((tokens (split-string addr ":"))
         (host (nth 0 tokens))
         (port (nth 1 tokens))
         (path ""))
    (when (not port)
      (setq port 22))
    (setq path (format "/sshx:%s#%s:~" host port))
    (message path)))

;; timestamp
(defun ts-to-human (ts)
  "Given ts INTEGER, return human readable string."
  (interactive "nts: ")
  (message (format-time-string "%Y-%m-%d %H:%M:%S" ts)))

(defun current-ts ()
  (interactive)
  (let ((ts
         (format "%s" (time-convert (current-time) 'integer))))
    (copy-variable-to-clipboard ts)))

;; hex to ascii, copied from stackoverflow
(defun decode-hex-string (hex-string)
  "Decode hex string convert HEX-STRING to ascii."
  (apply #'concat
         (cl-loop for i from 0 to (- (/ (length hex-string) 2) 1)
                  for hex-byte = (substring hex-string (* 2 i) (* 2 (+ i 1)))
                  collect (format "%c" (string-to-number hex-byte 16)))))

;; code toggle
;; from: https://emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
(defun jao-toggle-selective-display ()
  "Toggle overview."
  (interactive)
  (set-selective-display (if selective-display nil 1)))

(defun dos2unix ()
  "Not exactly but it's easier to remember."
  (interactive)
  (set-buffer-file-coding-system 'unix 't)
  (save-buffer))

(defun find-file-in-cpp-module ()
  "Find file in cpp module."
  (interactive)
  (find-file-in-current-directory 1))

(defun copy-variable-to-clipboard (val)
  "Copy VAL to both Emacs and system clipboard."
  (interactive)
  (with-temp-buffer
    (insert val)
    (mark-whole-buffer)
    (clipetty-kill-ring-save))
  (message "copied: %s" val))

(defun copy-relative-dir-in-project ()
  (interactive)
  (let ((path (file-name-directory
               (file-relative-name
                (buffer-file-name)
                (ffip-project-root)))))
    (copy-variable-to-clipboard path)))

(defun copy-relative-path-in-project ()
  (interactive)
  (let ((path (file-relative-name
               (buffer-file-name)
               (ffip-project-root))))
    (copy-variable-to-clipboard path)))

(defun copy-project-root-to-clipboard ()
  (interactive)
  (copy-variable-to-clipboard (ffip-project-root)))

(defun copy-current-dir-to-clipboard ()
  (interactive)
  (copy-variable-to-clipboard default-directory))

(defun copy-current-filename-to-clipboard ()
  (interactive)
  (copy-variable-to-clipboard
   (file-name-nondirectory buffer-file-name)))

(defun insert-zero-width-space ()
  "Insert zero width space before and after OrgMode mark."
  (interactive)
  (insert-char (char-from-name "ZERO WIDTH SPACE")))

(provide 'phye-init-utils)
