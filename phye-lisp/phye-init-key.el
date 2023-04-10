;; {{ global keymaps
(define-key global-map (kbd "C-x C-c") 'delete-frame)
(define-key global-map (kbd "C-x M") 'manual-entry)
(define-key global-map (kbd "M-`") 'other-frame)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c t") 'org-mark-ring-goto)
(define-key global-map (kbd "M-v") 'paste-from-x-clipboard)

(my-comma-leader-def
  "ls" 'highlight-symbol
  "ol" 'org-open-at-point
  "sl" 'org-store-link
  "il" 'org-insert-link
  "id" 'find-file-in-current-directory
  "ov" 'jao-toggle-selective-display
  "gt" 'lsp-find-definition
  "gr" 'lsp-find-references
  "dc" 'godoc-at-point
  "xb" 'ivy-switch-buffer
  "xc" 'suspend-frame
  "cc" 'clipetty-kill-ring-save
  "hd" 'hide-ifdef-block
  "hs" 'show-ifdef-block
  "ha" 'show-ifdefs
  "hb" 'hs-hide-block
  "hl" 'hs-hide-level
  "ho" 'hs-show-block)

(my-space-leader-def
  "rt" 'my-random-color-theme
  "nn" 'highlight-symbol-next
  "pp" 'highlight-symbol-prev)
;; }}

;; {{ mini buffer edit
(define-key minibuffer-local-map (kbd "C-a") 'move-beginning-of-line)
(define-key minibuffer-local-map (kbd "C-e") 'move-end-of-line)
(define-key minibuffer-local-map (kbd "C-w") 'evil-delete-backward-word)
(with-eval-after-load 'ivy-mode
  (define-key ivy-minibuffer-map (kbd "C-w") 'evil-delete-backward-word))
;; }}

(provide 'phye-init-key)
