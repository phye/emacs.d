;; diary
;; although I don't use Diary Mode, change the default file in case of mistyping
(setq diary-file "~/ws/gtd/diary.org")
(evil-set-initial-state 'image-mode 'emacs)
(evil-set-initial-state 'dired-mode 'normal)

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

;; open recent directory, requires ivy (part of swiper)
;; borrows from http://stackoverflow.com/questions/23328037/in-emacs-how-to-maintain-a-list-of-recent-directories
(defun bjm/ivy-dired-recent-dirs ()
  "Present a list of recently used directories and open the selected one in dired"
  (interactive)
  (let ((recent-dirs
         (delete-dups
          (mapcar (lambda (file)
                    (if (file-directory-p file) file (file-name-directory file)))
                  recentf-list))))

    (let ((dir (ivy-read "Directory: "
                         recent-dirs
                         :re-builder #'ivy--regex
                         :sort nil
                         :initial-input nil)))
      (dired dir))))

;; recentf
(use-package sync-recentf
  :ensure t
  :custom
  (recentf-auto-cleanup 60)
  :config
  (recentf-mode 1))

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

;; evil-matchit
(defun evilmi-customize-keybinding ()
  (evil-define-key 'normal evil-matchit-mode-map
    "%" 'evil-jump-item
    "m" 'evilmi-jump-items))

;; evil undo
(use-package evil
  :init
  (setq evil-undo-system 'undo-fu)
  :custom
  (evil-normal-state-cursor 'box)
  (evil-insert-state-cursor 'bar)
  (evil-emacs-state-cursor 'hbar)
  (evil-want-fine-undo 'fine))

(use-package evil-terminal-cursor-changer
  :ensure t
  :defer t)
(with-eval-after-load 'evil-terminal-cursor-changer
  (unless (display-graphic-p)
    (require 'evil-terminal-cursor-changer)
    (etcc-on)))

(use-package undo-fu)

(setq undo-limit 67108864) ; 64mb.
(setq undo-strong-limit 100663296) ; 96mb.
(setq undo-outer-limit 1006632960) ; 960mb.

(use-package undo-fu-session
  :ensure t
  :config
  (undo-fu-session-global-mode)
  (undo-fu-session-recover))

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

(defun phye/indent-after-newline (count)
  (indent-according-to-mode))

(advice-add 'evil-open-below
            :after #'phye/indent-after-newline)
(advice-add 'evil-open-above
            :after #'phye/indent-after-newline)

;; pangu spacing
(use-package pangu-spacing
  :ensure t
  :defer t
  :config
  :custom
  (pangu-spacing-separator " "))


;; deadgrep related
(use-package deadgrep
  :ensure t
  :defer t)

(use-package wgrep-deadgrep
  :ensure t
  :defer t)

(defun select-deadgrep-window-advice (search-term &optional directory)
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
  )

(defun phye/deadgrep-current-directory (search-term)
  "deadgrep in current directory"
  (interactive (list (deadgrep--read-search-term)))
  (deadgrep search-term default-directory))

(defun phye/project-find-dir ()
  "find directory fuzzily (copied from `'project-find-dir`'"
  (interactive)
  (let* ((project (project-current t))
         (all-files (project-files project))
         (completion-ignore-case read-file-name-completion-ignore-case)
         (all-dirs (mapcar #'file-name-directory all-files))
         (dir (funcall project-read-file-name-function
                       "Dired"
                       ;; Some completion UIs show duplicates.
                       (delete-dups all-dirs)
                       nil 'file-name-history)))
    dir))

(defun phye/deadgrep-directory ()
  "Find directory with fuzzy support, then restart the search"
  (interactive)
  (setq default-directory (phye/project-find-dir))
  (rename-buffer
   (deadgrep--buffer-name deadgrep--search-term default-directory)
   t)
  (deadgrep-restart))

(use-package project
  :config
  (setq project-switch-commands
        '((project-find-file "File")
          (project-find-regexp "Regexp")
          (project-find-dir "Dir")
          (project-switch-to-buffer "Buffer")
          (project-list-buffers "List buffers"))))

(with-eval-after-load 'counsel
  (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) ""))

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

(advice-add #'project-find-regexp :override #'deadgrep)

;; Do not use electric-pair-mode any more
(electric-pair-mode 0)
(use-package smartparens
  :ensure t ;; install the package
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  (require 'smartparens-config))

(provide 'phye-init-edit)
