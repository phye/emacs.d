;;; phye-init-key -- phye's favorite keybindings
;;; Commentary:
;;; do not use space to override keybinds in Emacs mode

;; {{ unbind unwanted keys
;; format: off
(general-unbind
 :keymaps '(image-mode-map doc-view-mode-map)
 "k")

;; format: off
(general-unbind
 :keymaps '(xref--xref-buffer-mode-map)
 ","
 ".")

;; format off
(general-unbind
 :keymaps '(helpful-mode-map)
 "C-w")
;; }}

;; {{ global keymaps

;; format: off
;; state independent global keys
(general-define-key
 :keymaps 'override
 "M-`" 'other-frame
 "M-v" 'paste-from-x-clipboard
 "C-a" 'move-beginning-of-line
 "C-e" 'move-end-of-line
 "C-p" 'previous-line
 "C-n" 'next-line
 "C-x C-c" 'delete-frame
 "C-x m" 'manual-entry
 "C-c a" 'org-agenda
 "C-c c" 'org-capture
 "C-c o" 'org-open-at-point
 "C-c t" 'org-mark-ring-goto)

;; format: off
;; state independent global keys with M-; prefix
(general-define-key
 :keymaps 'override
 :prefix "M-;"
 ";" 'ace-pinyin-jump-char-2
 "ff" 'my-toggle-full-window
 "jj" 'scroll-other-window-down
 "kk" 'scroll-other-window-up
 "<SPC>" 'insert-zero-width-space
 "<TAB>" 'insert-tab)

;; format: off
;; state dependent global keys
(general-define-key
 :states '(emacs)
 "C-w" 'backward-kill-word)

;; format: off
;; normal visual state without prefix
(general-define-key
 :states '(normal visual)
 "C-b" 'evil-scroll-up)

;; format: off
;; normal visual state with comma prefix
(general-define-key
 :states '(normal visual)
 :keymaps 'override
 :prefix ","
 "DD" 'counsel-etags-grep-current-directory
 "bb" 'phye/switch-to-previous-buffer-in-window
 "bm" 'phye/ace-move-buffer-to-window
 "bn" 'next-buffer
 "bp" 'previous-buffer
 "cD" 'copy-full-dir-to-clipboard
 "cF" 'copy-full-path-to-clipboard
 "cP" 'copy-project-root-to-clipboard
 "c[" 'annotate-goto-previous-annotation
 "c]" 'annotate-goto-next-annotation
 "ca" 'annotate-annotate
 "cb" 'org-mark-ring-goto
 "cc" 'clipetty-kill-ring-save
 "cd" 'copy-relative-dir-in-project
 "cf" 'cp-filename-of-current-buffer
 "co" 'org-open-at-point
 "cp" 'copy-project-name-to-clipboard
 "dc" 'phye/deadgrep-current-directory
 "dg" 'deadgrep
 "dk" 'deadgrep-kill-all-buffers
 "ee" 'eval-expression
 "fb" 'phye/format-buffer
 "fn" 'copy-file-name-to-clipboard
 "gC" 'phye/xref-clear-marker-stack
 "gb" 'phye/go-back-to-caller
 "gi" 'eglot-find-implementation
 "gr" 'xref-find-references
 "gt" 'phye/goto-definition-at-point
 "hB" 'hs-show-block
 "hF" 'counsel-describe-face
 "hI" 'show-ifdef-block
 "hL" 'hs-show-block
 "hS" 'hs-show-all
 "hb" 'hs-hide-block
 "hf" 'counsel-describe-function
 "hi" 'hide-ifdef-block
 "hl" 'hs-hide-level
 "hs" 'hs-hide-all
 "hv" 'counsel-describe-variable
 "id" 'find-file-in-current-directory
 "il" 'org-insert-link
 "im" 'counsel-imenu
 "ip" 'counsel-git
 "kmb" 'phye/kill-matching-buffers
 "le" 'flymake-show-buffer-diagnostics
 "ls" 'symbol-overlay-put
 "m." 'org-noter-sync-current-note
 "mG" 'bookmark-jump-other-frame
 "mS" 'bookmark-save
 "mX" 'execute-extended-command-for-buffer
 "mb" 'magit-blame
 "md" 'bookmark-delete
 "mf" 'iconify-frame
 "mg" 'bookmark-jump
 "mk" 'compile
 "mp" 'magit-push
 "ms" 'bookmark-set
 "mx" 'counsel-M-x
 "ne" 'flymake-goto-next-error
 "nn" 'narrow-to-region
 "oc" 'cfw:open-org-calendar
 "ov" 'jao-toggle-selective-display
 "pe" 'flymake-goto-prev-error
 "rR" 'phye/open-recent-file-in-other-frame
 "rc" 'recover-avy-lead-face
 "rd" 'bjm/ivy-dired-recent-dirs
 "rm" 'org-remark-mark
 "rn" 'org-remark-next
 "ro" 'phye/remark-view-and-select
 "rp" 'org-remark-prev
 "rD" 'org-remark-delete
 "rr" 'my-counsel-recentf
 "sc" 'agent-shell-send-region
 "sl" 'org-store-link
 "tt" 'shell-pop
 "ut" 'counsel-etags-update-tags-force
 "xB" 'ivy-switch-buffer
 "xK" 'phye/kill-buffer-and-frame
 "xb" 'bufler-switch-buffer
 "xc" 'suspend-frame
 "xd" 'find-file-in-cpp-module
 "xe" 'exit-recursive-edit
 "xf" 'find-file
 "xg" 'magit-status
 "xp!" 'project-shell
 "xpf" 'project-find-file
 "xpp" 'project-switch-project
 "xu" 'upcase-region
 "zC" 'outline-indent-close-folds
 "zO" 'outline-indent-open-folds
 "zc" 'outline-indent-close-fold
 "zo" 'outline-indent-open-fold
 "*" (lambda ()
             (interactive)
             (evil-Surround-region (region-beginning) (region-end) 'block ?*))
 "~" (lambda ()
             (interactive)
             (evil-Surround-region (region-beginning) (region-end) 'block ?~))
 "_" (lambda ()
             (interactive)
             (evil-Surround-region (region-beginning) (region-end) 'block ?_))
 "+" (lambda ()
             (interactive)
             (evil-Surround-region (region-beginning) (region-end) 'block ?+))
 "/" (lambda ()
             (interactive)
             (evil-Surround-region (region-beginning) (region-end) 'block ?/))
 "(" (lambda ()
             (interactive)
             (evil-Surround-region (region-beginning) (region-end) 'block ?\)))
 ")" (lambda ()
             (interactive)
             (evil-Surround-region (region-beginning) (region-end) 'block ?\)))
 "[" (lambda ()
             (interactive)
             (evil-Surround-region (region-beginning) (region-end) 'block ?\]))
 "]" (lambda ()
             (interactive)
             (evil-Surround-region (region-beginning) (region-end) 'block ?\])))

;; format: off
;; normal visual state with space prefix
(general-define-key
 :states '(normal visual)
 :prefix "SPC"
 "wd" 'dedicate-current-window
 "jj" 'evil-scroll-down
 "kk" 'evil-scroll-up
 "jh" 'phye/scroll-window-left
 "jl" 'phye/scroll-window-right
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
 "hr" 'phye/random-favorite-light-themes
 "hh" 'phye/random-all-themes
 "pc" 'popper-cycle
 "pl" 'popper-toggle-latest
 "vs" 'phye/vsplit-3-and-even)

;; format: off
;; motion state without prefix
(general-define-key
 :states 'motion
 "C-o" 'better-jumper-jump-backward
 "C-i" 'better-jumper-jump-forward)
;; }}

;; {{ mode specific map
;; format: off
(general-define-key
 :keymaps 'completion-preview-active-mode-map
 "M-n" 'completion-preview-next-candidate
 "M-p" 'completion-preview-prev-candidate
 "M-j" 'completion-preview-complete)

;; format: off
(general-define-key
 :keymaps '(image-mode-map doc-view-mode-map)
 "K" #'image-kill-buffer
 "q" #'quit-window
 "f" #'my-toggle-full-window)

;; format: off
(general-define-key
 :keymaps 'deadgrep-mode-map
 "n" 'deadgrep-forward-filename
 "p" 'deadgrep-backward-filename
 "j" 'next-line
 "k" 'previous-line
 "d" 'deadgrep-directory
 "D" 'phye/deadgrep-directory
 "RET" 'deadgrep-visit-result
 "C-x C-q" 'phye/wgrep-change-to-wgrep-mode
 "w" 'phye/wgrep-change-to-wgrep-mode)

;; format: off
(general-define-key
 :keymaps 'xref--xref-buffer-mode-map
 "n" 'xref-next-group
 "p" 'xref-prev-group
 "j" 'xref-next-line-no-show
 "k" 'xref-prev-line-no-show
 "o" 'xref-goto-xref
 "RET" 'phye/xref-goto-xref-and-quit)

;; format: off
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

;; format: off
(general-define-key
 :keymaps '(xref--xref-buffer-mode-map)
 :prefix ","
 "gg" 'evil-goto-first-line
 "G" 'evil-goto-line)

;; format: off
(general-define-key
 :keymaps 'org-remark-mode-map
 :states 'normal
 :prefix ","
 "q" 'delete-window)

;; format: off
(general-define-key
 :keymaps 'bookmark-minibuffer-read-name-map
 "C-w" 'evil-delete-backward-word)

;; format: off
(general-define-key
 :keymaps 'dired-mode-map
 "e" 'evil-forward-word-end
 "b" 'evil-backward-word-begin
 "h" 'dired-up-directory
 "j" 'dired-next-line
 "k" 'dired-previous-line
 "<RET>" 'phye/dired-open-file
 "C-o" 'casual-dired-tmenu)

;; format: off
(general-define-key
 :keymaps 'project-prefix-map
 "L" 'project-list-buffers)

;; format: off
(general-define-key
 :keymaps 'magit-blame-mode-map
 :prefix ","
 "mq" 'magit-blame-quit)

;; format: off
(general-define-key
 :keymaps '(sh-mode-map plantuml-mode-map)
 "<RET>" 'newline)

;; format: off
(general-define-key
 :keymaps 'agent-shell-mode-map
 "C-p" 'comint-previous-input
 "C-n" 'comint-next-input)

;; format: off
(general-define-key
 :keymaps 'Info-mode-map
 "C-o" 'evil-execute-in-normal-state)

;; format: off
(general-define-key
 :keymaps 'pdf-view-mode-map
 "C-s" 'pdf-occur
 "C-x o" 'ace-window
 ",xo" 'ace-window
 ",rR" 'phye/open-recent-file-in-other-frame
 "n" 'phye/pdf-goto-next-title-page
 "p" 'phye/pdf-goto-prev-title-page)
;; }}

;; {{ mini buffer edit
;; format: off
(general-define-key
 :keymaps 'minibuffer-mode-map
 "C-a" 'move-beginning-of-line
 "C-e" 'move-end-of-line
 "C-w" 'evil-delete-backward-word
 "C-p" 'previous-line-or-history-element
 "C-n" 'next-line-or-history-element)

;; format: off
(general-define-key
 :keymaps 'ivy-minibuffer-map
 "C-w" 'ivy-backward-kill-word)

;; format: off
(general-define-key
 :keymaps 'ivy-minibuffer-map
 :prefix ","
 "gg" 'ivy-beginning-of-buffer
 "G" 'ivy-end-of-buffer)
;; }}


(key-chord-define evil-insert-state-map ",," 'evil-escape)
(key-chord-define evil-insert-state-map "jk" 'evil-escape)

;; evil-matchit
(defun evilmi-customize-keybinding ()
       (evil-define-key 'normal evil-matchit-mode-map
                        "%" 'evil-jump-item
                        "m" 'evilmi-jump-items))

;; format: off
(defun phye/restore-keybindings ()
       "Restore keybindings by evil-nerd-commenter."
       (interactive)
       (my-comma-leader-def
        "cc" 'clipetty-kill-ring-save))
;; NOTE(phye); this is ugly... but simple and working for the moment ...
(my-run-with-idle-timer 5 'phye/restore-keybindings)

(provide 'phye-init-key)
