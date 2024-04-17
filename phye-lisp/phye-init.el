;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; General Edit Configs ;; ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst phye-lisp-dir (concat my-emacs-d "phye-lisp")
  "Directory of phye-lisp.")
(setq load-path (append (list phye-lisp-dir) load-path))

(require-package 'use-package)

(require 'phye-init-elpa)
(require 'phye-init-misc)
(require 'phye-init-edit)
(require 'phye-init-evil)
(require 'phye-init-themes)
(require 'phye-init-window)
(require 'phye-init-draw)
(require 'phye-init-org)
(require 'phye-init-doc)
(require 'phye-init-pdf)
(require 'phye-init-data)
(require 'phye-init-git)
(require 'phye-init-bin)
(require 'phye-init-prog)
(require 'phye-init-cc)
(require 'phye-init-utils)
(require 'phye-init-key)

(cd "~/ws")
(provide 'phye-init)
;;;;;;;;;;;;;;;
;; ;; End ;; ;;
;;;;;;;;;;;;;;;
