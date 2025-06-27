;;; phye-init-key -- phye's favorite keybindings
;;; Commentary:
;;; do not use space to override keybinds in Emacs mode

;; unbind unwanted keys
(general-unbind
  :keymaps '(image-mode-map doc-view-mode-map)
  "k")

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
 :states 'emacs
 :keymaps '(helpful-mode)
 "C-w h" 'evil-window-left
 "C-w l" 'evil-window-right
 "C-w j" 'evil-window-down
 "C-w k" 'evil-window-up)

(general-define-key
 "s-q" 'delete-frame
 "s-Q" 'server-shutdown)

(general-define-key
 :states '(insert normal emacs)
 :prefix "C-;"
 ";" 'ace-pinyin-jump-char-2
 "ff" 'my-toggle-full-window
 "<SPC>" 'insert-zero-width-space
 "<TAB>" 'insert-tab)

(general-define-key
 :states '(normal visual)
 "C-b" 'evil-scroll-up)

(general-define-key
 :states '(normal visual)
 :prefix ","
 "bb" 'phye/switch-to-previous-buffer-in-window
 "bp" 'previous-buffer
 "bn" 'next-buffer
 "fb" 'phye/format-buffer
 "gr" 'xref-find-references
 "gb" 'phye/go-back-to-caller
 "gt" 'phye/goto-definition-at-point
 "gC" 'phye/xref-clear-marker-stack
 "gi" 'eglot-find-implementation
 "hf" 'counsel-describe-function
 "hv" 'counsel-describe-variable
 "hF" 'counsel-describe-face
 "hs" 'hs-hide-all
 "hS" 'hs-show-all
 "hl" 'hs-hide-level
 "hL" 'hs-show-block
 "hb" 'hs-hide-block
 "hB" 'hs-show-block
 "hi" 'hide-ifdef-block
 "hI" 'show-ifdef-block
 "il" 'org-insert-link
 "ls" 'symbol-overlay-put
 "nn" 'narrow-to-region
 "oc" 'cfw:open-org-calendar
 "co" 'org-open-at-point
 "ov" 'jao-toggle-selective-display
 "rc" 'recover-avy-lead-face
 "rm" 'phye/mark-and-open
 "rp" 'org-remark-prev
 "rn" 'org-remark-next
 "ro" 'phye/remark-view-and-select
 "xd" 'find-file-in-cpp-module
 "xe" 'exit-recursive-edit
 "*" (lambda () (interactive) (evil-Surround-region (region-beginning) (region-end) 'block ?*))
 "~" (lambda () (interactive) (evil-Surround-region (region-beginning) (region-end) 'block ?~))
 "_" (lambda () (interactive) (evil-Surround-region (region-beginning) (region-end) 'block ?_))
 "+" (lambda () (interactive) (evil-Surround-region (region-beginning) (region-end) 'block ?+))
 "/" (lambda () (interactive) (evil-Surround-region (region-beginning) (region-end) 'block ?/))
 "(" (lambda () (interactive) (evil-Surround-region (region-beginning) (region-end) 'block ?\)))
 ")" (lambda () (interactive) (evil-Surround-region (region-beginning) (region-end) 'block ?\)))
 "[" (lambda () (interactive) (evil-Surround-region (region-beginning) (region-end) 'block ?\]))
 "]" (lambda () (interactive) (evil-Surround-region (region-beginning) (region-end) 'block ?\]))
 )

(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 "wd" 'dedicate-current-window
 "fD" 'delete-frame
 "fn" 'phye/select-next-frame
 "fp" 'phye/select-previous-frame
 "fr" 'set-frame-name
 "fs" 'select-frame-by-name
 "fo" 'find-file-other-frame
 "mc" 'phye/maximize-center-window
 "nn" 'symbol-overlay-jump-next
 "os" 'org-side-tree
 "pp" 'symbol-overlay-jump-prev
 "rt" 'my-random-favorite-color-theme
 "rl" 'phye/random-favorite-light-themes
 "hh" 'phye/random-all-themes
 "pc" 'popper-cycle
 "pl" 'popper-toggle-latest
 "vs" 'phye/vsplit-3-and-even)

(general-define-key
 :states 'motion
 "C-o" 'better-jumper-jump-backward
 "C-i" 'better-jumper-jump-forward)
;; }}

;; {{ override map
(general-define-key
 :states '(normal visual)
 :keymaps 'override
 :prefix ","
 "cc" 'clipetty-kill-ring-save
 "cd" 'copy-relative-dir-in-project
 "cf" 'copy-relative-path-in-project
 "cD" 'copy-full-dir-to-clipboard
 "cF" 'copy-full-path-to-clipboard
 "cp" 'copy-project-name-to-clipboard
 "cP" 'copy-project-root-to-clipboard
 "cn" 'copy-file-name-to-clipboard
 "dg" 'deadgrep
 "dc" 'phye/deadgrep-current-directory
 "dk" 'deadgrep-kill-all-buffers
 "DD" 'counsel-etags-grep-current-directory
 "ee" 'eval-expression
 "ip" 'find-file-in-project
 "id" 'find-file-in-current-directory
 "im" 'counsel-imenu
 "kmb" 'phye/kill-matching-buffers
 "le" 'flymake-show-buffer-diagnostics
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
 "xb" 'ivy-switch-buffer
 "xc" 'suspend-frame
 "xf" 'find-file
 "xB" 'project-switch-to-buffer
 "xK" 'phye/kill-buffer-and-frame
 "xg" 'magit-status
 "xpp" 'project-switch-project
 "xpf" 'project-find-file
 "xp!" 'project-shell)

(general-define-key
 :states '(normal)
 :keymaps 'override
 :prefix ";"
 ";" 'ace-pinyin-jump-char-2)

(general-define-key
 :states '(normal)
 :keymaps 'override
 "tt" 'ace-pinyin-jump-char-2)

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
 :keymaps 'completion-preview-active-mode-map
 "M-n" 'completion-preview-next-candidate
 "M-p" 'completion-preview-prev-candidate
 "M-j" 'completion-preview-complete)

(general-define-key
 :keymaps '(image-mode-map doc-view-mode-map)
 "K" #'image-kill-buffer
 "q" #'quit-window
 "f" #'my-toggle-full-window)

(general-define-key
 :keymaps 'deadgrep-mode-map
 "n" 'deadgrep-forward-filename
 "p" 'deadgrep-backward-filename
 "j" 'next-line
 "k" 'previous-line
 "d" 'deadgrep-directory
 "D" 'phye/deadgrep-directory
 "RET" 'deadgrep-visit-result-other-window
 "C-x C-q" 'phye/wgrep-change-to-wgrep-mode
 "w" 'phye/wgrep-change-to-wgrep-mode)

(general-define-key
 :keymaps '(
            neotree-mode-map
            deadgrep-mode-map
            pdf-outline-buffer-mode-map
            xref--xref-buffer-mode-map
            pdf-view-mode-map
            pdf-occur-buffer-mode-map)
 :prefix ";"
 ";" 'ace-pinyin-jump-char-2)

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
 "C-w" 'ivy-backward-kill-word)

(general-define-key
 :keymaps 'xref--xref-buffer-mode-map
 "n" 'xref-next-group
 "p" 'xref-prev-group
 "j" 'xref-next-line
 "k" 'xref-prev-line)

(general-define-key
 :keymaps 'dired-mode-map
 "h" 'dired-up-directory
 "l" 'dired-find-file
 "j" 'dired-next-line
 "k" 'dired-previous-line
 "<RET>" 'phye/dired-open-file
 "C-o" 'casual-dired-tmenu)

(general-define-key
 :keymaps 'project-prefix-map
 "L" 'project-list-buffers)

(general-define-key
 :keymaps 'ivy-minibuffer-map
 :prefix ","
 "gg" 'ivy-beginning-of-buffer
 "G" 'ivy-end-of-buffer)

(general-define-key
 :keymaps 'magit-blame-mode-map
 :prefix ","
 "mq" 'magit-blame-quit)

(general-define-key
 :keymaps 'bookmark-minibuffer-read-name-map
 "C-w" 'evil-delete-backward-word)

(general-define-key
 :keymaps '(sh-mode-map plantuml-mode-map)
 "<RET>" 'newline)

;; evil-matchit
(defun evilmi-customize-keybinding ()
  (evil-define-key 'normal evil-matchit-mode-map
    "%" 'evil-jump-item
    "m" 'evilmi-jump-items))

(general-define-key
 :keymaps 'Info-mode-map
 "C-o" 'evil-execute-in-normal-state)

(general-define-key
 :keymaps 'pdf-view-mode-map
 "C-s" 'pdf-occur
 "C-x o" 'ace-window
 ",xo" 'ace-window
 ",rR" 'phye/open-recent-file-in-other-frame
 "n" 'phye/pdf-goto-next-title-page
 "p" 'phye/pdf-goto-prev-title-page)

(provide 'phye-init-key)
