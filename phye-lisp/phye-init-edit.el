;; diary
;; although I don't use Diary Mode, change the default file in case of mistyping
(setq diary-file "~/ws/gtd/diary.org")

(setq my-disable-wucuo t)
;; clipboard
(use-package clipetty
  :ensure t
  :defer t
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
(setq recentf-save-file "~/.emacs.data.d/recentf")

(use-package undo-fu
  :ensure t
  :defer t)

(setq undo-limit 67108864) ; 64mb.
(setq undo-strong-limit 100663296) ; 96mb.
(setq undo-outer-limit 1006632960) ; 960mb.

(use-package undo-fu-session
  :ensure t
  :defer t
  :config
  (undo-fu-session-global-mode)
  (undo-fu-session-recover))

(my-run-with-idle-timer
 2 ;; gpg encrypt
 (lambda ()
   (require 'epa-file)
   (epa-file-enable)
   (recentf-load-list)))

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
  :custom
  (project-list-file "~/.emacs.data.d/projects")
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
  :defer t
  :config
  (better-jumper-mode +1))

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
  :defer t
  :hook (prog-mode text-mode markdown-mode) ;; add `smartparens-mode` to these hooks
  :config
  (require 'smartparens-config))

(defun phye/cleanup-white-spaces ()
  "Delete white spaces between two Chinese characters."
  (interactive)
  (save-excursion
    (mark-whole-buffer)
    (replace-regexp-in-region
     "\\(\\cc\\) \\(\\cc\\)"
     "\\1\\2"
     (region-beginning)
     (region-end))))

(provide 'phye-init-edit)
