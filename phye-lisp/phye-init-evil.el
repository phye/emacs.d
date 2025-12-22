(evil-set-initial-state 'image-mode 'emacs)
(evil-set-initial-state 'hexl-mode 'emacs)
(evil-set-initial-state 'helpful-mode 'emacs)
(evil-set-initial-state 'dired-mode 'normal)
(evil-set-initial-state 'godoc-mode 'normal)
(evil-set-initial-state 'agent-shell-mode 'emacs)

;; evil customizations
(use-package evil-escape :ensure t :custom (evil-escape-delay 0.2) (evil-escape-key-sequence "fd"))

;; (use-package evil-numbers
;;   :ensure t
;;   :defer t
;;   :bind (:map evil-normal-state-map ("C-a" . evil-numbers/inc-at-pt)))

;; evil undo
(use-package
 evil
 :init (setq evil-undo-system 'undo-fu)
 :custom
 (evil-normal-state-cursor 'box)
 (evil-insert-state-cursor 'bar)
 (evil-emacs-state-cursor 'hbar)
 (evil-want-fine-undo t)
 (undo-fu-allow-undo-in-region t)
 (undo-fu-ignore-keyboard-quit t))

(use-package evil-terminal-cursor-changer :ensure t :defer t)
(with-eval-after-load 'evil-terminal-cursor-changer
  (unless (display-graphic-p)
    (require 'evil-terminal-cursor-changer)
    (etcc-on)))

(defun phye/indent-after-newline (count)
  (indent-according-to-mode))

(advice-add 'evil-open-below :after #'phye/indent-after-newline)
(advice-add 'evil-open-above :after #'phye/indent-after-newline)

(provide 'phye-init-evil)
