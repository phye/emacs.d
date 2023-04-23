(setq phye-elpa-packages
      '(
        ;; lsp-treemacs
        annotate-mode
        clipetty
        dockerfile-mode
        emacsql
        helpful
        lsp-ivy
        lsp-mode
        lsp-ui
        org-journal
        org-present
        org-roam
        peep-dired
        popper
        rg
        sync-recentf
        vimish-fold
        ))
(setq melpa-include-packages
      (append melpa-include-packages phye-elpa-packages nil))

(provide 'phye-init-elpa)