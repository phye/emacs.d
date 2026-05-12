;;; phye-init-git.el --- Git and version control configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Magit settings and URL conversion helpers.

;;; Code:

(declare-function magit-status "magit")
(declare-function magit-blame "magit")
(declare-function magit-push "magit")
(declare-function exec-path-from-shell-copy-env "exec-path-from-shell")
(declare-function ediff-cleanup-mess "ediff-util")

(setq vc-follow-symlinks t)

;; use magit to edit commit message
(with-eval-after-load 'magit
  t)

;; To use this function:
;; 1. Select a region containing HTTPS Git URLs.
;; 2. M-x convert-https-to-ssh-in-region

(defun convert-https-to-ssh-in-region (start end)
  "Convert HTTPS Git URLs to SSH format in the selected region START to END."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward "https://\\([^/]+\\)/\\([^[:space:]\n]+\\)" end t)
      (let ((domain (match-string 1))
            (repo-path (match-string 2)))
        (replace-match (format "git@%s:%s" domain repo-path) t t)))))

(defun phye/convert-https-to-ssh-in-git-config ()
  "Convert HTTPS git URLs to SSH in git-config."
  (interactive)
  (convert-https-to-ssh-in-region (point-min) (point-max)))

(use-package code-review-minimal
  :vc (:url "https://github.com/phye/code-review-minimal"
            :rev "exp"))

;;; Ediff abort mechanism
;; "Q" aborts the git difftool session: ediff-really-quit cleans up, then
;; server-send-string sends "-error" to terminate the emacsclient with code 1;
;; ediff.sh propagates that exit code and git difftool's trustExitCode stops
;; remaining files.  "q" quits cleanly (exit 0).

(defun phye/ediff-abort ()
  "Abort the entire git difftool session.
Quits ediff then sends \"-error\" via the server protocol so emacsclient
exits with code 1.  git difftool's trustExitCode stops remaining files."
  (interactive)
  (let ((proc (frame-parameter (selected-frame) 'client)))
    (ediff-really-quit nil)
    (when proc
      (server-send-string proc "-error Ediff aborted\n"))))

(defun phye/ediff-setup ()
  "Configure the live ediff control buffer.
Captures frame, binds \"Q\" to abort and \"q\" to quit without prompt.
Called from `ediff-startup-hook' where `ediff-mode-map' is already active."
  (let ((frame (selected-frame)))
    (add-hook 'ediff-quit-hook (lambda () (delete-frame frame)) t t))
  (define-key ediff-mode-map (kbd "Q") #'phye/ediff-abort)
  (define-key ediff-mode-map (kbd "q") (lambda () (interactive) (ediff-really-quit nil))))

(defun phye/ediff-start (fn &rest args)
  "Start an ediff session.
Registers `phye/ediff-setup' on `ediff-startup-hook' then calls FN with ARGS."
  (add-hook 'ediff-startup-hook #'phye/ediff-setup t)
  (apply fn args))

(provide 'phye-init-git)

;;; phye-init-git.el ends here
