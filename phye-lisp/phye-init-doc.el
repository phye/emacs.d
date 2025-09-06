(defun
 phye/markdown-hook () "diasable trunc lines" (interactive)
 ;;(linum-mode)
 (setq truncate-lines nil))

(use-package
 markdown-mode
 :ensure t
 :defer t
 :config (add-hook 'markdown-mode-hook 'phye/markdown-hook 90))

;; org mode export github style markdown
(use-package
 ox-gfm
 :ensure t
 :defer t
 :config (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode)))

(use-package
 markdown-toc
 :ensure t
 :defer t
 :custom
 (markdown-toc-header-toc-title "")
 (markdown-toc-indentation-space 2)
 (markdown-toc-user-toc-structure-manipulation-fn #'cdr))

(defun
 cc/markdown-to-org-region
 (start end)
 "Convert Markdown formatted text in region (START, END) to Org.

This command requires that pandoc (man page `pandoc(1)') be
installed."
 (interactive "r")
 (shell-command-on-region start end "pandoc -f markdown -t org --wrap=preserve" t t))

;; {{ info mode
(defun
 phye/info-mode-hook
 ()
 "bind evil like windmov"
 (interactive)
 (define-key Info-mode-map (kbd "C-w h") 'evil-window-left)
 (define-key Info-mode-map (kbd "C-w l") 'evil-window-right)
 (define-key Info-mode-map (kbd "C-w j") 'evil-window-down)
 (define-key Info-mode-map (kbd "C-w k") 'evil-window-up))
;; (add-hook 'Info-mode-hook #'phye/info-mode-hook 90)
;; }}

(provide 'phye-init-doc)
