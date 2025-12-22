(setq phye-elpa-packages
      '(activities
        acp
        agent-shell
        annotate-mode
        bazel
        better-jumper
        bpftrace-mode
        bufler
        burly
        clipetty
        deadgrep
        dockerfile-mode
        doric-themes
        el-themes
        elisp-autofmt
        emacs-everywhere
        emacsql
        evil-collection
        evil-terminal-cursor-changer
        halloweenie-theme
        hcl-mode
        helpful
        key-chord
        markdown-toc
        mermaid-mode
        nerd-icons
        ob-mermaid
        org-fancy-priorities
        org-journal
        org-noter
        org-present
        org-remark
        org-roam
        org-side-tree
        outline-indent
        peep-dired
        pikchr-mode
        popper
        pumpkin-spice-theme
        puni
        rg
        show-font
        symbol-overlay
        sync-recentf
        tiny
        treemacs
        undo-fu-session
        vimish-fold))

(setq melpa-include-packages (append melpa-include-packages phye-elpa-packages nil))

(provide 'phye-init-elpa)
