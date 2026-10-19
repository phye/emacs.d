;;; phye-init-data.el --- Data format modes  -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuration for JSON, YAML, protobuf, and Dockerfile modes.

;;; Code:

(declare-function json-mode "json-mode")
(declare-function outline-indent-minor-mode "outline-indent")
(declare-function outline-hide-subtree "outline")
(declare-function outline-show-subtree "outline")
(declare-function phye/prog-mode-hook "phye-init-prog")

;; {{ JavaScript/JSON
(use-package
 json-mode
 :ensure t
 :defer t
 :custom
 (js-indent-level 2)
 (json-encoding-default-indentation "  ")
 :config
 (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
 (add-hook 'json-mode-hook #'hs-minor-mode))
;; }}

(defun cb/pretty-print-yaml-json (beg end)
  "Parse region (BEG END) as a double-quoted string and pretty-print as JSON."
  (interactive "r")
  (let* ((input-str (buffer-substring-no-properties beg end))
         (quoted-str
          (if (eq (aref input-str 0) ?\")
              input-str
            (format "\"%s\"" input-str)))
         (unescaped-json (car (read-from-string quoted-str)))
         (json-object (json-read-from-string unescaped-json))
         (json-encoding-pretty-print t)
         (pretty-json (json-encode json-object)))
    ;; Output the result
    (with-current-buffer (get-buffer-create "*JSON Pretty Print*")
      (erase-buffer)
      (insert pretty-json)
      (json-mode) ;; Use json-mode if available for syntax highlighting
      (pop-to-buffer (current-buffer)))))

;; {{ YAML
;; from: https://github.com/yoshiki/yaml-mode/issues/25
(defun phye/yaml-mode-hook ()
  "My yaml mode hook."
  ;; (ts-fold-mode t)
  (outline-indent-minor-mode))

(defun phye/yaml--current-indent ()
  "Return indentation column of the current line."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (current-column)))

(defun phye/yaml--line-flavor ()
  "Classify current line as :key, :list, :template, :comment, or :blank.
The classification looks at the first non-whitespace character."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (cond
     ((eolp) :blank)
     ((eq (char-after) ?-) :list)
     ((eq (char-after) ?\{) :template)
     ((eq (char-after) ?#) :comment)
     (t :key))))

(defun phye/yaml--map-siblings (fn)
  "Call FN with point on each sibling of the current line under the same parent.
Siblings are lines that share the current line's indentation AND flavor
\(see `phye/yaml--line-flavor'\) and belong to the same enclosing key
parent, or all top-level lines when the current line has no key parent."
  (save-excursion
    (let ((child-indent (phye/yaml--current-indent))
          (child-flavor (phye/yaml--line-flavor))
          (beg (point-min))
          (end (point-max))
          (parent-indent -1))
      (beginning-of-line)
      ;; Walk upward for the nearest KEY line with smaller indent.
      (while (and (= parent-indent -1) (not (bobp)))
        (forward-line -1)
        (when (eq (phye/yaml--line-flavor) :key)
          (let ((i (phye/yaml--current-indent)))
            (when (< i child-indent)
              (setq parent-indent i)
              (forward-line 1)
              (setq beg (point))))))
      ;; Walk downward to find where the parent's subtree ends.
      (goto-char beg)
      (while (and (not (eobp))
                  (or (looking-at-p "^[ \t]*$")
                      (> (phye/yaml--current-indent) parent-indent)))
        (forward-line 1))
      (setq end (point))
      ;; Apply FN to every same-indent, same-flavor line in [beg, end).
      (goto-char beg)
      (while (< (point) end)
        (when (and (eq (phye/yaml--line-flavor) child-flavor)
                   (= (phye/yaml--current-indent) child-indent))
          (save-excursion (funcall fn)))
        (forward-line 1)))))

(defun phye/yaml-fold-same-level ()
  "Fold every YAML node at the current indentation level under the same parent."
  (interactive)
  (phye/yaml--map-siblings #'outline-hide-subtree))

(defun phye/yaml-unfold-same-level ()
  "Unfold every YAML node at the current indentation level under the same parent."
  (interactive)
  (phye/yaml--map-siblings #'outline-show-subtree))

(use-package
 yaml-mode
 :ensure t
 :defer t
 :mode (".yaml$")
 :hook
 (yaml-mode . display-line-numbers-mode)
 (yaml-mode . phye/yaml-mode-hook))
;; }}

;; {{ protobuf
(use-package
 protobuf-mode
 :ensure t
 :defer t
 :config (add-hook 'protobuf-mode-hook 'phye/prog-mode-hook 90))
;; }}

;; conf
(add-to-list 'auto-mode-alist '("\\.txt\\'" . conf-mode))

;; log
(add-to-list 'auto-mode-alist '("\\.log\\'" . log-view-mode))

;; Dockerfile
(use-package
 dockerfile-mode
 :ensure t
 :defer t
 :config (add-to-list 'auto-mode-alist '("Dockerfile_" . dockerfile-mode)))

(provide 'phye-init-data)
;;; phye-init-data.el ends here
