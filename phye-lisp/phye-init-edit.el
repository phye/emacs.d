;; diary
;; although I don't use Diary Mode, change the default file in case of mistyping
(setq diary-file "~/ws/gtd/diary.org")

(setq my-disable-wucuo t)
(set-fill-column 100)
;; clipboard
(use-package clipetty
  :ensure t
  :custom
  (clipetty-tmux-ssh-tty "tmux show-environment SSH_TTY"))
;; }}

;; file and dirs
;; preview files in dired
(use-package peep-dired
  :ensure t
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

;; recentf
(use-package sync-recentf
   :ensure t
   :custom
   (recentf-auto-cleanup 60)
   :config
   (recentf-mode 1))


;; Don't pair double quotes
;; https://emacs.stackexchange.com/questions/26225/dont-pair-quotes-in-electric-pair-mode
(defun phye/set-electric-pair-inhibit-predicate()
  "set electric-pair-inhibit-predicate "
  (interactive)
  (setq electric-pair-inhibit-predicate
    (lambda (c)
      (if (or
           (char-equal c ?\{)
           (char-equal c ?\[)
           (char-equal c ?\()
           (char-equal c ?\')
           (char-equal c ?\"))
          t
        (electric-pair-default-inhibit c)))))
(with-eval-after-load 'elec-pair
  ;; (phye/set-electric-pair-inhibit-predicate)
  )

;; evil customizations
(use-package evil-escape
  :ensure t
  :custom
  (evil-escape-delay 0.2)
  (evil-escape-key-sequence "fd"))

;; (use-package evil-numbers
;;   :ensure t
;;   :defer t
;;   :bind (:map evil-normal-state-map ("C-a" . evil-numbers/inc-at-pt)))

;; gpg encrypt
(require 'epa-file)
(epa-file-enable)

;; camelCase, snake_case .etc
(use-package string-inflection
  :ensure t
  :defer t
  :config
  (define-key global-map (kbd "C-c i") 'string-inflection-cycle)
  (define-key global-map (kbd "C-c C") 'string-inflection-camelcase)
  (define-key global-map (kbd "C-c L") 'string-inflection-lower-camelcase))

(use-package linum-relative
  :ensure t
  :defer t)

(use-package crux
  :ensure t
  :defer t)

(use-package aggressive-indent
  :ensure t
  :defer t)

(use-package ialign
  :ensure t
  :defer t)

(use-package tiny
  :ensure t
  :defer t)

(use-package rg
  :ensure t
  :defer t)

(defun phye/indent-after-newline (COUNT)
  (indent-according-to-mode))

(advice-add 'evil-open-below
            :after #'phye/indent-after-newline)
(advice-add 'evil-open-above
            :after #'phye/indent-after-newline)

(defun select-rg-window-advice (pattern files dir &optional literal confirm flags)
  "Select rg buffer"
  (select-window (get-buffer-window "*rg*")))
(advice-add 'rg-run
            :after-until #'select-rg-window-advice)

(with-eval-after-load 'rg-mode
  (define-key rg-mode-map (kbd ";") 'ace-pinyin-jump-char-2)
  (define-key rg-mode-map (kbd "j") 'next-line)
  (define-key rg-mode-map (kbd "k") 'previous-line)
  )

(use-package deadgrep
  :ensure t
  :defer t)

(defun select-deadgrep-window-advice (SEARCH_TERM &optional DIRECTORY)
  "Select deadgrep buffer"
  (select-window (get-buffer-window "*deadgrep\\.*")))
(advice-add 'deadgrep
            :after-until #'select-deadgrep-window-advice)

(with-eval-after-load 'deadgrep
  ;; (define-key deadgrep-mode-map (kbd ";") 'ace-pinyin-jump-char-2)
  (unbind-key (kbd ";")  'deadgrep-mode-map)
  (general-define-key
   :keymaps 'deadgrep-mode-map
   :prefix ";"
   ";" 'ace-pinyin-jump-char-2)
  (general-define-key
   :keymaps 'deadgrep-mode-map
   "n" 'deadgrep-forward-filename
   "p" 'deadgrep-backward-filename
   "j" 'next-line
   "k" 'previous-line
   "RET" 'deadgrep-visit-result-other-window
   "C-w h" 'evil-window-left
   "C-w l" 'evil-window-right
   "C-w j" 'evil-window-down
   "C-w k" 'evil-window-up))

;; optional if you want which-key integration
;; (use-package which-key
;;     :config
;;     (which-key-mode))
;; }}

(with-eval-after-load 'company-ispell
  (setq company-ispell-available nil)
  )

(use-package better-jumper
  :ensure t
  :config
  (better-jumper-mode +1)
  (with-eval-after-load 'evil-maps
    (define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
    (define-key evil-motion-state-map (kbd "C-i") 'better-jumper-jump-forward)))
(defun phye/deadgrep-visit-result-hook ()
  (interactive)
  (better-jumper-set-jump))
(advice-add 'deadgrep-visit-result
            :after #'phye/deadgrep-visit-result-hook)
(advice-add 'deadgrep-visit-result-other-window
            :after #'phye/deadgrep-visit-result-hook)

(provide 'phye-init-edit)