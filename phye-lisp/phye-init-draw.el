;; {{ plantuml
(use-package plantuml-mode
    :ensure t
    :defer t
    :custom
    (org-plantuml-jar-path "~/.emacs.d/misc/plantuml.jar")
    (plantuml-jar-path "~/.emacs.d/misc/plantuml.jar")
    (plantuml-default-exec-mode 'jar)
    (plantuml-indent-level 0)
    :config
    (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode)))
;; }}

;; {{ graphviz-dot-mode
(use-package graphviz-dot-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.dot\\'" . graphviz-dot-mode)))
(add-hook 'graphviz-dot-mode-hook #'turn-off-auto-fill)
;; }}

;; {{ artist
(defun artist-mode-toggle-emacs-state ()
  (if artist-mode
      (evil-emacs-state)
    (evil-exit-emacs-state)))
(add-hook 'artist-mode-hook #'artist-mode-toggle-emacs-state)
;; }}

;; {{ gnuplot
(use-package gnuplot
  :ensure t
  :defer t)
;;}}
(provide 'phye-init-draw)