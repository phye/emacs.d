;;; phye-init.el --- Main entry point for phye's Emacs config  -*- lexical-binding: t; -*-

;;; Commentary:
;; Loads all phye-init-* modules in order.

;;; Code:

(defconst phye-lisp-dir (concat my-emacs-d "phye-lisp")
  "Directory of phye-lisp.")
(setq load-path (append (list phye-lisp-dir) load-path))

(require-package 'use-package)

(require 'phye-init-elpa)
(require 'phye-init-misc)
(require 'phye-init-utils)
(require 'phye-init-evil)
(require 'phye-init-draw)
(require 'phye-init-doc)
(require 'phye-init-pdf)
(require 'phye-init-data)
(require 'phye-init-bin)
(require 'phye-init-window)
(require 'phye-init-edit)
(require 'phye-init-prog)
(require 'phye-init-cc)
(require 'phye-init-org)
(require 'phye-init-ai)
(require 'phye-init-git)
(require 'phye-init-themes)
(require 'phye-init-key)

(cd "~/ws")
(provide 'phye-init)
;;; phye-init.el ends here
