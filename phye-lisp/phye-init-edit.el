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

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package crux
  :ensure t
  :defer t)

(use-package aggressive-indent
  :ensure t
  :defer t)

;; optional if you want which-key integration
;; (use-package which-key
;;     :config
;;     (which-key-mode))
;; }}

(provide 'phye-init-edit)