(setq phye-elpa-packages
      '(org-journal
        vimish-fold
        emacsql
        org-roam
        org-present
        popper
        lsp-ui
        lsp-ivy
        ;; lsp-treemacs
        lsp-mode
        dockerfile-mode
        sync-recentf
        clipetty
        annotate-mode
        peep-dired
        rg
        helpful))
(setq melpa-include-packages
      (append melpa-include-packages phye-elpa-packages nil))

(provide 'phye-init-elpa)