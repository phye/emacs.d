(setq
 phye-elpa-packages
 '(better-jumper
   treemacs
   annotate-mode
   clipetty
   deadgrep
   dockerfile-mode
   elisp-autofmt
   emacsql
   emacs-everywhere
   hcl-mode
   helpful
   nerd-icons
   key-chord
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
   org-side-tree
   pumpkin-spice-theme
   halloweenie-theme
   el-themes
   undo-fu-session
   ;; book-mode ;; not in melpa yet
   evil-collection
   evil-terminal-cursor-changer
   markdown-toc
   org-fancy-priorities
   outline-indent
   mermaid-mode
   ob-mermaid
   symbol-overlay
   doric-themes
   puni
   bpftrace-mode))

(setq melpa-include-packages (append melpa-include-packages phye-elpa-packages nil))


(provide 'phye-init-elpa)
