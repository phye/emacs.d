;;; phye-init-utils.el --- Utility functions for phye's config  -*- lexical-binding: t; -*-

;;; Commentary:
;; Miscellaneous utility functions.

;;; Code:

(require 'cl-lib)

(declare-function recentf-save-list "recentf")
(declare-function clipetty-kill-ring-save "clipetty")
(declare-function find-file-in-current-directory "find-file-in-project")
(declare-function ffip-project-root "find-file-in-project")

;; define function to shutdown emacs server instance

(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (recentf-save-list)
  (bookmark-save)
  (kill-emacs))

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
  "Given TS INTEGER, return human readable string."
  (interactive "nts: ")
  (message (format-time-string "%Y-%m-%d %H:%M:%S" ts)))

(defun current-ts ()
  "Copy the current Unix timestamp as an integer to clipboard."
  (interactive)
  (let ((ts (format "%s" (time-convert (current-time) 'integer))))
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
  (set-selective-display
   (if selective-display
       nil
     1)))

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
  "Copy the relative directory of the current file within the project to clipboard."
  (interactive)
  (let ((path (file-name-directory (file-relative-name (buffer-file-name) (ffip-project-root)))))
    (copy-variable-to-clipboard path)))

(defun copy-relative-path-in-project ()
  "Copy the relative path of the current file within the project to clipboard."
  (interactive)
  (let ((path (file-relative-name (buffer-file-name) (ffip-project-root))))
    (copy-variable-to-clipboard path)))

(defun copy-project-root-to-clipboard ()
  "Copy the project root directory to clipboard."
  (interactive)
  (copy-variable-to-clipboard (ffip-project-root)))

(defun copy-full-dir-to-clipboard ()
  "Copy the full directory of the current buffer to clipboard."
  (interactive)
  (copy-variable-to-clipboard default-directory))

(defun copy-full-path-to-clipboard ()
  "Copy the full path of the current file to clipboard."
  (interactive)
  (copy-variable-to-clipboard (file-truename buffer-file-name)))

(defun copy-file-name-to-clipboard ()
  "Copy the file name (without directory) of the current buffer to clipboard."
  (interactive)
  (copy-variable-to-clipboard (file-name-nondirectory buffer-file-name)))

(defun get-last-dirname (path)
  "Get the last directory name from the given PATH."
  (file-name-nondirectory (directory-file-name (file-name-directory path))))

(defun copy-project-name-to-clipboard ()
  "Copy the project name (last component of project root) to clipboard."
  (interactive)
  (copy-variable-to-clipboard (get-last-dirname (ffip-project-root))))

(defun insert-zero-width-space ()
  "Insert zero width space."
  (interactive)
  (insert-char (char-from-name "ZERO WIDTH SPACE")))

(defun phye/insert-tab ()
  "Insert tab."
  (interactive)
  (insert-char ?\t))

(defun capi/count-apis ()
  "Count APIs by greping lines starting with '  /'."
  (interactive)
  (message "APIs: %d" (how-many "^  /" (point-min) (point-max))))

(defun proto/count-rpcs ()
  "Count number of RPCs in the wrapping service or in the whole buffer."
  (interactive)
  (let ((p (point))
        (beg (point-min))
        (end (point-max))
        (scope "buffer"))
    (save-excursion
      (when (re-search-backward "^\\s-*service\\s-+\\(\\w+\\)" nil t)
        (let ((name (match-string 1))
              (start (point)))
          (when (search-forward "{" nil t)
            (backward-char)
            (condition-case nil
                (progn
                  (forward-list)
                  (when (> (point) p)
                    (setq beg start
                          end (point)
                          scope (format "service %s" name))))
              (error nil))))))
    (message "RPCs in %s: %d" scope (how-many "^\\s-*rpc\\b" beg end))))

;;;###autoload
(defun phye/protobuf-jump-req-rsp ()
  "Jump between req and rsp in current line if there're req/rsp at point.
Otherwise, jump to the next req/rsp matched."
  (interactive)
  (let (current-name other-name bounds (origin (point)))
    (setq bounds (bounds-of-thing-at-point 'symbol))
    (when bounds
      (setq current-name (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (message "current-name at point: %s" current-name)

    (cond
     ((and current-name (string-suffix-p "Req" current-name))
      (setq other-name (concat (substring current-name 0 -3) "Rsp")))
     ((and current-name (string-suffix-p "Rsp" current-name))
      (setq other-name (concat (substring current-name 0 -3) "Req")))
     (t
      ;; Not on a Req/Rsp symbol: jump forward to the next one
      (unless (re-search-forward "\\b\\w+\\(?:Req\\|Rsp\\)\\b" nil t)
        (user-error "No Req/Rsp found forward in buffer"))
      (goto-char (match-beginning 0))))

    (when other-name
      (goto-char (point-min))
      (if (re-search-forward other-name nil t)
          (goto-char (match-beginning 0))
        (goto-char origin)
        (user-error "Could not find %s" other-name)))))

(provide 'phye-init-utils)
;;; phye-init-utils.el ends here
