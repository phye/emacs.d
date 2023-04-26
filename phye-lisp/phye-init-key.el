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
(define-key global-map (kbd "C-x C-g") 'aboabo/hydra-git-gutter/body)

(evil-define-key 'normal 'magit-blame-mode-map "q" #'magit-blame-quit)

(my-comma-leader-def
  "cc" 'clipetty-kill-ring-save
  "cd" 'copy-relative-path-in-project
  "dc" 'godoc-at-point
  "dg" 'deadgrep
  "fb" 'clang-format-buffer
  "ft" 'clang-format
  "mb" 'magit-blame
  "gr" 'lsp-find-references
  "gt" 'lsp-find-definition
  "ha" 'show-ifdefs
  "hb" 'hs-hide-block
  "hd" 'hide-ifdef-block
  "hl" 'hs-hide-level
  "ho" 'hs-show-block
  "hs" 'show-ifdef-block
  "id" 'find-file-in-current-directory
  "il" 'org-insert-link
  "ls" 'highlight-symbol
  "nn" 'highlight-symbol-next
  "ol" 'org-open-at-point
  "ov" 'jao-toggle-selective-display
  "pc" 'popper-cycle
  "pl" 'popper-toggle-latest
  "pp" 'highlight-symbol-prev
  "rg" 'projectile-ripgrep
  "sl" 'org-store-link
  "tt" 'shell-pop
  "xb" 'ivy-switch-buffer
  "xc" 'suspend-frame
  "xd" 'find-file-in-cpp-module
  "xe" 'exit-recursive-edit)

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
