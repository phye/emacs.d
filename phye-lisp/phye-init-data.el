;; {{ JavaScript/JSON
(use-package json-mode
  :ensure t
  :defer t
  :custom
  (js-indent-level 2)
  (json-encoding-default-indentation "  ")
  :config
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))
  (add-hook 'json-mode-hook #'hs-minor-mode))
;; }}

;; {{ YAML
;; from: https://github.com/yoshiki/yaml-mode/issues/25
(use-package yaml-mode
  :ensure t
  :defer t
  :mode (".yaml$")
  :hook
  (yaml-mode . yaml-mode-outline-hook)

  :init
  (defun yaml-outline-level ()
    "Return the outline level based on the indentation, hardcoded at 2 spaces."
    (s-count-matches "[ ]\\{2\\}" (match-string 0)))

  (defun yaml-mode-outline-hook ()
    (outline-minor-mode t)
    (setq outline-regexp
          (rx
           (seq
            bol
            (group (zero-or-more "  ")
                   (or (group
                        (seq (or (seq "\"" (*? (not (in "\"" "\n"))) "\"")
                                 (seq "'" (*? (not (in "'" "\n"))) "'")
                                 (*? (not (in ":" "\n"))))
                             ":"
                             (?? (seq
                                  (*? " ")
                                  (or (seq "&" (one-or-more nonl))
                                      (seq ">-")
                                      (seq "|"))
                                  eol))))
                       (group (seq
                               "- "
                               (+ (not (in ":" "\n")))
                               ":"
                               (+ nonl)
                               eol)))))))
    (setq outline-level 'yaml-outline-level)))
;; }}

;; {{ protobuf
(use-package protobuf-mode
  :ensure t
  :defer t
  :config
  (add-hook 'protobuf-mode-hook 'phye/prog-mode-hook 90))
;; }}

;; conf
(add-to-list 'auto-mode-alist '("\\.txt\\'" . conf-mode))

;; log
(add-to-list 'auto-mode-alist '("\\.log\\'" . log-view-mode))

;; Dockerfile
(use-package dockerfile-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile_" . dockerfile-mode))
  )

(provide 'phye-init-data)