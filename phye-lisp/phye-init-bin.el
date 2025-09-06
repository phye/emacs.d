;; {{ binary
(evil-set-initial-state 'hexl-mode 'emacs)

(defun phye/hexl-mode-hook ()
  "phye's hexl mode hook"
  (interactive)
  (read-only-mode t))

(use-package
 hexl
 :defer t
 :config
 (add-to-list 'auto-mode-alist '("\\.bin\\'" . hexl-mode))
 (add-to-list 'auto-mode-alist '("\\.data\\'" . hexl-mode))
 (add-to-list 'auto-mode-alist '("\\.o\\'" . hexl-mode))
 (define-key hexl-mode-map (kbd "n") 'hexl-forward-char)
 (define-key hexl-mode-map (kbd "e") 'hexl-forward-short)
 (define-key hexl-mode-map (kbd "E") 'hexl-forward-word)
 (define-key hexl-mode-map (kbd "p") 'hexl-backward-char)
 (define-key hexl-mode-map (kbd "b") 'hexl-backward-short)
 (define-key hexl-mode-map (kbd "B") 'hexl-backward-word)
 :hook (hexl-mode . phye/hexl-mode-hook))
;; }}

;; {{ ASM
(add-to-list 'auto-mode-alist '("\\.dump\\'" . asm-mode))
;; }}

(provide 'phye-init-bin)
