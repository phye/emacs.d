(setq phye-elpa-packages
      '(
        better-jumper
        treemacs
        annotate-mode
        clipetty
        deadgrep
        dockerfile-mode
        emacsql
        helpful
        org-journal
        org-present
        org-roam
        org-remark
        org-noter
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
