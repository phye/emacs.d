(defun phye/load-theme (theme)
  "Load THEME after disable custom themes."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(if (display-graphic-p)
  (phye/load-theme 'doom-monokai-classic)
  (phye/load-theme 'kaolin-galaxy))

(customize-set-variable
 'my-favorite-color-themes
 '(afternoon
   atom-dark
   atom-one-dark
   ample
   ample-zen
   bubbleberry
   clues
   cherry-blossom
   cyberpunk
   dakrone
   darkmine
   darkokai
   doom-1337
   doom-Iosvkem
   doom-acario-dark
   doom-badger
   doom-challenger-deep
   doom-city-lights
   doom-dark+
   doom-dracula
   doom-ephemeral
   doom-feather-dark
   doom-gruvbox
   doom-homage-black
   doom-horizon
   doom-ir-black
   doom-laserwave
   doom-miramare
   doom-molokai
   doom-monokai-classic
   doom-monokai-machine
   doom-monokai-octagon
   doom-monokai-pro
   doom-monokai-ristretto
   doom-nord
   doom-nord-aurora
   doom-nova
   doom-oceanic-next
   doom-old-hope
   doom-opera
   doom-palenight
   doom-sourcerer
   doom-spacegrey
   doom-vibrant
   doom-xcode
   doom-zenburn
   dorsey
   ef-bio
   ef-elea-dark
   ef-dark
   ef-autumn
   gotham
   grandshell
   gruvbox-dark-soft
   gruvbox-dark-medium
   hc-zenburn
   hemisu-dark
   jazz
   kaolin-aurora
   kaolin-eclipse
   kaolin-bubblegum
   kaolin-dark
   kaolin-galaxy
   kaolin-ocean
   kaolin-temple
   kaolin-valley-dark
   manoj-dark
   madhat2r
   modus-vivendi
   nord
   noctilux
   occidental
   omtose-darker
   planet
   purple-haze
   rebecca
   sanityinc-tomorrow-eighties
   sanityinc-tomorrow-night
   seti
   solarized-selenized-black
   solarized-gruvbox-dark
   solarized-wombat-dark
   spacemacs-dark
   srcery
   tango-dark
   wombat))

(customize-set-variable
 'my-favorite-light-color-themes
 '(apropospriate-light
   doom-feather-light
   doom-homage-white
   doom-nord-light
   doom-oksolar-light
   doom-one-light
   doom-opera-light
   kaolin-valley-light
   kaolin-mono-light
   ef-deuteranopia-light
   ef-duo-light
   ef-elea-light
   ef-frost
   ef-maris-light
   ef-melissa-light
   ef-trio-light
   ef-tritanopia-light
   farmhouse-light
   professional
   twilight-bright
   whiteboard))

(defun phye/random-all-themes ()
  "Random all color themes."
  (interactive)
  (my-pickup-random-color-theme (custom-available-themes)))

(defun phye/random-favorite-light-themes ()
  "Random my favorite light themes."
  (interactive)
  (my-pickup-random-color-theme my-favorite-light-color-themes))

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
     ("CANCELLED" . "#d0bf8f")
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
  "Recovy avy leader face."
  (interactive)
  (require 'avy)
  (set-face-attribute 'avy-lead-face nil :inherit 'font-lock-warning-face))
(with-eval-after-load 'avy
  (recover-avy-lead-face))

(advice-add 'my-random-favorite-color-theme :after #'recover-avy-lead-face)
(advice-add 'my-random-healthy-color-theme :after #'recover-avy-lead-face)
(advice-add 'my-random-color-themes :after #'recover-avy-lead-face)

(unless (boundp 'font-lock-reference-face)
  (defface font-lock-reference-face
    '((t :inherit t
         :weight bold))
    "add missing font-lock-reference-face")
  (defvar font-lock-reference-face 'font-lock-reference-face))

(use-package beacon
  :ensure t
  :defer t
  :custom
  (beacon-blink-duration 0.2)
  :config
  (beacon-mode 1))

(use-package hl-anything
  :ensure t
  :defer t
  :config
  (hl-global-highlight-on/off))

(use-package ef-themes
  :ensure t
  :defer t)

(defvar previous-dark-theme 'cyberpunk "Previous dark theme before toggle.")

(defun phye/set-bg-color (&optional light)
  "Set ivy-current-match color based on LIGHT."
  (interactive)
  (let ((bg-color ""))
    (if light
        (setq bg-color "#00FF86")
      (setq bg-color "#0065FF"))
    (custom-set-faces
     `(ivy-current-match ((t (:extend t :background ,bg-color)))))))

(defun phye/toggle-theme (&optional light)
  "Toggle light theme if LIGHT is t, restore dark theme otherwise."
  (interactive)
  (let ((loc (getenv "LOCATION")))
    (message "Toggle Theme at %s" loc)
    (when (equal loc "office")
      (if light
          (progn
            (setq previous-dark-theme (car custom-enabled-themes))
            (my-random-healthy-color-theme))
        (phye/load-theme previous-dark-theme))
      (phye/set-bg-color light)
      (when (display-graphic-p)
        (when (eq (selected-frame) (car (frame-list)))
          (shell-command
           (format "~/bin/scripts/toggle_dark_theme.sh %s"
                   (if light "false" "true"))))))))
(run-at-time "09:30" 86400 #'(lambda () (phye/toggle-theme t)))
(run-at-time "12:00" 86400 #'(lambda () (phye/toggle-theme nil)))

(let ((light t))
  (shell-command "~/bin/scripts/toggle_dark_theme.sh" (if light "false" "true")))

(provide 'phye-init-themes)
