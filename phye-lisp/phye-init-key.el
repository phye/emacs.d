;; {{ global keymaps
(define-key global-map (kbd "C-x C-c") 'delete-frame)
(define-key global-map (kbd "C-x M") 'manual-entry)
(define-key global-map (kbd "M-`") 'other-frame)
(define-key global-map (kbd "C-c c") 'org-capture)
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c t") 'org-mark-ring-goto)
(define-key global-map (kbd "M-v") 'paste-from-x-clipboard)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c l") 'org-store-link)

(my-comma-leader-def
  "fb" 'clang-format-buffer
  "ft" 'clang-format
  "ls" 'highlight-symbol
  "ol" 'org-open-at-point
  "sl" 'org-store-link
  "il" 'org-insert-link
  "id" 'find-file-in-current-directory
  "xd" 'find-file-in-cpp-module
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
  "ho" 'hs-show-block
  "tt" 'shell-pop
  "nn" 'highlight-symbol-next
  "pp" 'highlight-symbol-prev
  "pc" 'popper-cycle
  "pl" 'popper-toggle-latest
  "cd" 'copy-relative-path-in-project
  "xe" 'exit-recursive-edit
  "gg" 'projectile-ripgrep
  )

(my-space-leader-def
  "nn" 'highlight-symbol-next
  "pp" 'highlight-symbol-prev
  "rt" 'my-random-favorite-color-theme
  "hh" 'my-random-healthy-color-theme)
;; }}

;; {{ mini buffer edit
(define-key minibuffer-local-map (kbd "C-a") 'move-beginning-of-line)
(define-key minibuffer-local-map (kbd "C-e") 'move-end-of-line)
(defun phye/ivy-mode-hook ()
  (define-key ivy-minibuffer-map (kbd "C-w") 'evil-delete-backward-word))
(add-hook 'ivy-mode-hook 'phye/ivy-mode-hook)
;; }}

(provide 'phye-init-key)
