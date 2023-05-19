;; {{ global keymaps
(general-define-key
 "M-`" 'other-frame
 "M-v" 'paste-from-x-clipboard

 "C-x C-c" 'delete-frame
 "C-x m" 'manual-entry

 "C-c a" 'org-agenda
 "C-c c" 'org-capture
 "C-c o" 'org-open-at-point
 "C-c t" 'org-mark-ring-goto)

(general-define-key
 :states 'insert
 :prefix "C-;"
 ";" 'ace-pinyin-jump-char-2)

(my-comma-leader-def
  "cc" 'clipetty-kill-ring-save
  "cd" 'copy-relative-path-in-project
  "dg" 'deadgrep
  "dc" 'phye/deadgrep-current-directory
  "dk" 'deadgrep-kill-all-buffers
  "fb" 'clang-format-buffer
  "mb" 'magit-blame
  "mk" 'compile
  "gr" 'lsp-find-references
  "gb" 'xref-pop-marker-stack
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
  "co" 'org-open-at-point
  "ov" 'jao-toggle-selective-display
  "sl" 'org-store-link
  "tt" 'shell-pop
  "xb" 'ivy-switch-buffer
  "xc" 'suspend-frame
  "xd" 'find-file-in-cpp-module
  "xe" 'exit-recursive-edit
  "xg" 'magit-status
  "xp" 'project-switch-project)

(defun phye/restore-keybindings ()
  "Restore keybindings by evil-nerd-commenter."
  (interactive)
  (my-comma-leader-def
    "cc" 'clipetty-kill-ring-save))
;; NOTE(phye); this is ugly... but simple and working for the moment ...
(my-run-with-idle-timer 5 'phye/restore-keybindings)

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
 :keymaps 'image-mode-map
 "q" #'quit-window
 "f" #'my-toggle-full-window)

(general-define-key
 :keymaps 'deadgrep-mode-map
 "n" 'deadgrep-forward-filename
 "p" 'deadgrep-backward-filename
 "j" 'next-line
 "k" 'previous-line
 "D" 'phye/deadgrep-directory
 "RET" 'deadgrep-visit-result-other-window
 "C-w h" 'evil-window-left
 "C-w l" 'evil-window-right
 "C-w j" 'evil-window-down
 "C-w k" 'evil-window-up)

(general-define-key
 :keymaps 'rg-mode-map
 ";" 'ace-pinyin-jump-char-2
 "j" 'next-line
 "k" 'previous-line)
;; }}

;; {{ mini buffer edit
(general-define-key
 :keymaps 'minibuffer-mode-map
 "C-a" 'move-beginning-of-line
 "C-e" 'move-end-of-line
 "C-w" 'evil-delete-backward-word)

(general-define-key
 :keymaps 'ivy-minibuffer-map
 "C-w" 'evil-delete-backward-word)

(provide 'phye-init-key)
