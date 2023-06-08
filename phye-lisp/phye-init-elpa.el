(setq phye-elpa-packages
      '(
        ;; lsp-treemacs
        better-jumper
        treemacs
        annotate-mode
        clipetty
        deadgrep
        dockerfile-mode
        emacsql
        helpful
        lsp-ivy
        lsp-mode
        lsp-ui
        org-journal
        org-present
        org-roam
        org-remark
        peep-dired
        popper
        rg
        tiny
        sync-recentf
        tiny
        vimish-fold
        ))
(setq melpa-include-packages
      (append melpa-include-packages phye-elpa-packages nil))

(provide 'phye-init-elpa)
