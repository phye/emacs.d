;;; phye-init-key -- phye's favorite keybindings
;;; Commentary:
;;; do not use space to override keybinds in Emacs mode

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
 "s-q" 'delete-frame
 "s-Q" 'server-shutdown)

(general-define-key
 :states '(insert normal)
 :prefix "C-;"
 ";" 'ace-pinyin-jump-char-2
 "<SPC>" 'insert-zero-width-space)

(general-define-key
 :states '(normal visual)
 :prefix ","
  "bb" 'phye/switch-to-previous-buffer-in-window
  "fb" 'clang-format-buffer
  "gr" 'xref-find-references
  "gb" 'xref-pop-marker-stack
  "gt" 'phye/goto-definition-at-point
  "gi" 'eglot-find-implementation
  "hs" 'hs-hide-all
  "hS" 'hs-show-all
  "hl" 'hs-hide-level
  "hb" 'hs-hide-block
  "hB" 'hs-show-block
  "hi" 'hide-ifdef-block
  "hI" 'show-ifdef-block
  "il" 'org-insert-link
  "ls" 'highlight-symbol
  "oc" 'cfw:open-org-calendar
  "co" 'org-open-at-point
  "ov" 'jao-toggle-selective-display
  "xd" 'find-file-in-cpp-module
  "xe" 'exit-recursive-edit)

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 "wd" 'dedicate-current-window
 "wu" 'undedicate-current-window
 "fD" 'delete-frame
 "fn" 'phye/select-next-frame
 "fp" 'phye/select-previous-frame
 "fr" 'set-frame-name
 "fs" 'select-frame-by-name
 "fo" 'find-file-other-frame
 "ff" 'phye/toggle-last-frame
 "ft" 'my-toggle-full-window
 "mc" 'phye/maximize-center-window
 "nn" 'highlight-symbol-next
 "os" 'org-side-tree
 "pp" 'highlight-symbol-prev
 "rt" 'my-random-favorite-color-theme
 "rp" 'org-remark-prev
 "rn" 'org-remark-next
 "rm" 'phye/mark-and-open
 "ro" 'phye/remark-view-and-select
 "hh" 'phye/random-all-themes
 "pc" 'popper-cycle
 "pl" 'popper-toggle-latest)
;; }}

;; {{ override map
(general-define-key
 :states '(emacs normal visual)
 :keymaps 'override
 :prefix ","
 "cc" 'clipetty-kill-ring-save
 "cd" 'copy-relative-dir-in-project
 "cf" 'copy-relative-path-in-project
 "dg" 'deadgrep
 "dc" 'phye/deadgrep-current-directory
 "dk" 'deadgrep-kill-all-buffers
 "ee" 'eval-expression
 "ip" 'find-file-in-project
 "id" 'find-file-in-current-directory
 "kmb" 'phye/kill-matching-buffers
 "mb" 'magit-blame
 "mk" 'compile
 "mp" 'magit-push
 "ms" 'bookmark-set
 "mS" 'bookmark-save
 "mg" 'bookmark-jump
 "mG" 'bookmark-jump-other-frame
 "md" 'bookmark-delete
 "mx" 'counsel-M-x
 "mX" 'execute-extended-command-for-buffer
 "rd" 'bjm/ivy-dired-recent-dirs
 "rr" 'my-counsel-recentf
 "rR" 'phye/open-recent-file-in-other-frame
 "sl" 'org-store-link
 "tt" 'shell-pop
 "ut" 'counsel-etags-update-tags-force
 "xb" 'project-switch-to-buffer
 "xc" 'suspend-frame
 "xf" 'find-file
 "xB" 'ivy-switch-buffer
 "xK" 'phye/kill-buffer-and-frame
 "xg" 'magit-status
 "xpp" 'project-switch-project
 "xpf" 'project-find-file
 "xp!" 'project-shell)

(general-define-key
 :states '(emacs normal)
 :keymaps 'override
 :prefix ";"
 ";" 'ace-pinyin-jump-char-2)
;; }}

(defun phye/restore-keybindings ()
  "Restore keybindings by evil-nerd-commenter."
  (interactive)
  (my-comma-leader-def
    "cc" 'clipetty-kill-ring-save))
;; NOTE(phye); this is ugly... but simple and working for the moment ...
(my-run-with-idle-timer 5 'phye/restore-keybindings)

;; {{ mode specific map
(general-define-key
 :states 'normal
 :keymaps 'go-mode-map
 :prefix ","
 "fb" 'gofmt)

(general-define-key
 :keymaps '(image-mode-map doc-view-mode-map)
 "K" #'image-kill-buffer
 "q" #'quit-window
 "f" #'my-toggle-full-window)

(general-unbind
  :keymaps '(image-mode-map doc-view-mode-map)
  "k")

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

(general-define-key
 :keymaps 'org-remark-mode-map
 :states 'normal
 :prefix ","
 "q" 'delete-window)

(general-define-key
 :keymaps 'helpful-mode-map
 "f" 'my-toggle-full-window)
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

(general-define-key
 :keymaps 'xref--xref-buffer-mode-map
 "j" 'xref-next-line
 "k" 'xref-prev-line)

(general-define-key
 :keymaps 'dired-mode-map
 "h" 'dired-up-directory
 "l" 'dired-find-file
 "j" 'dired-next-line
 "k" 'dired-previous-line)

(general-define-key
 :keymaps 'project-prefix-map
 "L" 'project-list-buffers)

(provide 'phye-init-key)
