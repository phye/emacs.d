(setq vc-follow-symlinks t)

;; use magit to edit commit message
(require 'magit)

(defun
 convert-https-to-ssh-in-region
 (start end)
 "Convert HTTPS Git URLs to SSH format in the selected region."
 (interactive "r")
 (save-excursion
  (goto-char start)
  (while
   (re-search-forward "https://\\([^/]+\\)/\\([^[:space:]\n]+\\)" end t)
   (let ((domain (match-string 1))
         (repo-path (match-string 2)))
     (replace-match (format "git@%s:%s" domain repo-path) t t)))))

;; To use this function:
;; 1. Select a region containing HTTPS Git URLs.
;; 2. M-x convert-https-to-ssh-in-region

(provide 'phye-init-git)
