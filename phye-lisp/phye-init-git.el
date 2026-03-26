;;; phye-init-git.el --- Git and version control configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Magit settings and URL conversion helpers.

;;; Code:

(declare-function magit-status "magit")
(declare-function magit-blame "magit")
(declare-function magit-push "magit")
(declare-function exec-path-from-shell-copy-env "exec-path-from-shell")

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

(use-package
 gf-code-review
 :load-path "~/.emacs.d/site-lisp/gf-code-review/"
 :config
 (exec-path-from-shell-copy-env "GIT_WOA_TOKEN")
 (gf-code-review-set-token
  (encode-coding-string (string-trim (getenv "GIT_WOA_TOKEN")) 'us-ascii)))

(provide 'phye-init-git)

;;; phye-init-git.el ends here
