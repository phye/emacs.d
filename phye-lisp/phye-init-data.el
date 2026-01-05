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
  "Parse the region as a double-quoted string (handling YAML/Lisp style escapes),parse the result as JSON, and pretty-print it to a new buffer."
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
