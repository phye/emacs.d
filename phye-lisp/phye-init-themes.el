(load-theme 'spacemacs-dark t)
(setq my-favorite-color-themes
      '(srcery
        atom-dark
        atom-one-dark
        doom-dark+
        doom-Iosvkem
        doom-acario-dark
        doom-challenger-deep
        doom-dracula
        doom-gruvbox
        doom-ir-black
        doom-molokai
        doom-monokai-classic
        doom-monokai-machine
        doom-monokai-octagon
        doom-monokai-pro
        doom-monokai-ristretto
        doom-monokai-spectrum
        doom-material-dark
        doom-gruvbox
        doom-xcode
        doom-nova
        doom-nord
        doom-nord-aurora
        doom-material-dark
        doom-oceanic-next
        doom-old-hope
        doom-opera
        doom-zenburn
        doom-palenight
        doom-spacegrey
        tango-dark
        ;; solarized-dark-high-contrast
        ;; sanityinc-solarized-dark
        sanityinc-tomorrow-eighties
        sanityinc-tomorrow-night
        modus-vivendi
        spacemacs-dark
        seti
        planet
        dakrone
        doom-city-lights
        doom-material
        kaolin-galaxy
        kaolin-bubblegum
        kaolin-temple
        cyberpunk
        ;; vscode-dark-plus
        ))

(use-package hl-todo
  :ensure t
  :defer t
  :custom
  (hl-todo-keyword-faces
   '(("STUB" . "#1E90FF")
     ("Deprecated" . "white")
     ("PITFALL" . "#FF4500")
     ("LOGIC" . "yellow")
     ("PURPOSE" . "lavender")
     ("THOUGHT" . "orange")
     ("DEBUG" . "blue")
     ("HOLD" . "#d0bf8f")
     ("TODO" . "#cc9393")
     ("NEXT" . "#dca3a3")
     ("THEM" . "#dc8cc3")
     ("PROG" . "#7cb8bb")
     ("OKAY" . "#7cb8bb")
     ("DONT" . "#5f7f5f")
     ("FAIL" . "#8c5353")
     ("DONE" . "#afd8af")
     ("NOTE" . "#d0bf8f")
     ("KLUDGE" . "#d0bf8f")
     ("HACK" . "#d0bf8f")
     ("TEMP" . "#d0bf8f")
     ("FIXME" . "#cc9393")
     ("XXX+" . "#cc9393"))))

(customize-save-variable
 'highlight-symbol-colors
 '("red"                                ;; red
   "white"                              ;; white
   "green"                              ;; green
   "gray"                               ;; gray
   "blue"                               ;; blue
   "yellow"                             ;; yellow
   "magenta"                            ;; red
   "ivory"                              ;; white
   "cyan"                               ;; green
   "thistle"                            ;; gray
   "navy"                               ;; blue
   "gold"                               ;; yellow
   "maroon"                             ;; red
   "lavender"                           ;; white
   "turquoise"                          ;; green
   "slate gray"                         ;; gray
   "slate blue"                         ;; blue
   "moccasin"                           ;; yellow
   "salmon"                             ;; red
   "snow"                               ;; white
   "chartreuse"                         ;; green
   "honeydew"                           ;; gray
   "dodger blue"                        ;; blue
   "khaki"                              ;; yellow
   "violet red"                         ;; red
   "navajo white"                       ;; white
   "light sea green"                    ;; green
   "light slate gray"                   ;; gray
   "deep sky blue"                      ;; blue
   "light coral"                        ;; yellow
   ))

;; customize avy jump colors
(defun recover-avy-lead-face ()
  (interactive)
  (require 'avy)
  (set-face-attribute 'avy-lead-face nil :foreground "red")
  (set-face-attribute 'avy-lead-face nil :background "navy")
  (set-face-attribute 'avy-lead-face-0 nil :foreground "magenta")
  (set-face-attribute 'avy-lead-face-0 nil :background "green"))
(with-eval-after-load 'avy
  (recover-avy-lead-face))

(advice-add 'my-random-favorite-color-theme :after #'recover-avy-lead-face)
(advice-add 'my-random-healthy-color-theme :after #'recover-avy-lead-face)
(advice-add 'my-random-color-themes :after #'recover-avy-lead-face)

(defun phye/clean-symbol (&optional symbol)
  (interactive)
  "Filter symbol without leading $,@ characters"
  (let* ((symbol (or symbol
                     (symbol-at-point)
                    (error "No symbol at point")))
         (symbolstr (symbol-name symbol))
         (c (elt symbolstr 0)))
    (if (or (= c ?$)
            (= c ?@))
        (list (seq-subseq symbolstr 1)) ;; return symbol without first char
      (list symbolstr))))
;; (advice-add 'highlight-symbol :filter-args #'phye/clean-symbol)

(provide 'phye-init-themes)