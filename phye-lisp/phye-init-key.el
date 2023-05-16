;; {{ global keymaps
;; (define-key global-map (kbd "C-x C-g") 'aboabo/hydra-git-gutter/body)

(general-define-key
 "M-`" 'other-frame
 "M-v" 'paste-from-x-clipboard

 "C-x C-c" 'delete-frame
 "C-x m" 'manual-entry

 "C-c a" 'org-agenda
 "C-c c" 'org-capture
 "C-c o" 'org-open-at-point
 "C-c t" 'org-mark-ring-goto)

(my-comma-leader-def
  "cc" 'clipetty-kill-ring-save
  "cd" 'copy-relative-path-in-project
  "dc" 'godoc-at-point
  "dg" 'deadgrep
  "dk" 'deadgrep-kill-all-buffers
  "fb" 'clang-format-buffer
  "mb" 'magit-blame
  "mk" 'compile
  "gr" 'lsp-find-references
  "gt" 'phye/goto-definition-at-point
  "hs" 'hs-show-all
  "hh" 'hs-hide-all
  "hl" 'hs-hide-level
  "hbh" 'hs-hide-block
  "hbs" 'hs-show-block
  "hdh" 'hide-ifdef-block
  "hds" 'show-ifdef-block
  "id" 'find-file-in-current-directory
  "il" 'org-insert-link
  "ls" 'highlight-symbol
  "oc" 'cfw:open-org-calendar
  "ol" 'org-open-at-point
  "ov" 'jao-toggle-selective-display
  "sl" 'org-store-link
  "tt" 'shell-pop
  "xb" 'ivy-switch-buffer
  "xc" 'suspend-frame
  "xd" 'find-file-in-cpp-module
  "xe" 'exit-recursive-edit
  "xg" 'magit-status)

(my-space-leader-def
  "fD" 'delete-frame
  "fn" 'phye/select-next-frame
  "fp" 'phye/select-previous-frame
  "fr" 'set-frame-name
  "fs" 'select-frame-by-name
  "fo" 'find-file-other-frame
  "ff" 'phye/toggle-last-frame
  "ft" 'my-toggle-full-window
  "nn" 'highlight-symbol-next
  "pp" 'highlight-symbol-prev
  "rt" 'my-random-favorite-color-theme
  "hh" 'my-random-healthy-color-theme
  "pc" 'popper-cycle
  "pl" 'popper-toggle-latest
  )
;; }}

;; {{ mode specific map
(general-define-key
 :states 'normal
 :keymaps 'magit-blame-mode-map
 "q" #'magit-blame-quit)

(general-define-key
 :states 'normal
 :keymaps 'go-mode-map
 :prefix ","
 "fb" 'gofmt)

(general-define-key
 :states 'insert
 :prefix "C-;"
 ";" 'ace-pinyin-jump-char-2)

(general-define-key
 :keymaps 'image-mode-map
 "q" #'quit-window)
;; }}

;; {{ mini buffer edit
(general-define-key
 :keymaps 'minibuffer-mode-map
 "C-a" 'move-beginning-of-line
 "C-e" 'move-end-of-line
 "C-w" 'evil-delete-backward-word)

(defun phye/ivy-mode-hook ()
  (general-define-key
   :keymaps 'ivy-minibuffer-map
   "C-w" 'evil-delete-backward-word))
(add-hook 'ivy-mode-hook 'phye/ivy-mode-hook)
;; }}

(provide 'phye-init-key)
