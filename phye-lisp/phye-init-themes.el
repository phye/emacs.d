(defun phye/load-theme (theme)
  "Load THEME after disable custom themes."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

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
   doom-lantern
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
   doric-dark
   doric-fire
   dorsey
   ef-autumn
   ef-bio
   ef-cherie
   ef-elea-dark
   ef-dark
   ef-duo-dark
   ef-night
   ef-owl
   ef-rosa
   ef-winter
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
   modus-vivendi-tritanopia
   nord
   noctilux
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
   smyx
   tango-dark
   wombat
   zerodark))

(customize-set-variable
 'my-favorite-light-color-themes
 '(apropospriate-light
   doom-bluloco-light
   doom-feather-light
   doom-homage-white
   doom-nord-light
   doom-oksolar-light
   doom-one-light
   doom-opera-light
   doric-cherry
   doric-earth
   doric-light
   doric-wind
   doric-marble
   kaolin-valley-light
   kaolin-mono-light
   kaolin-breeze
   ef-arbutus
   ef-deuteranopia-light
   ef-duo-light
   ef-eagle
   ef-elea-light
   ef-frost
   ef-kassio
   ef-maris-light
   ef-melissa-light
   ef-spring
   ef-summer
   ef-trio-light
   ef-tritanopia-light
   farmhouse-light
   occidental
   professional
   spacemacs-light
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

(use-package
 hl-todo
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
    ("TBD" . "#cc9494")
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
 '("red" ;; red
   "white" ;; white
   "green" ;; green
   "gray" ;; gray
   "blue" ;; blue
   "yellow" ;; yellow
   "magenta" ;; red
   "ivory" ;; white
   "cyan" ;; green
   "thistle" ;; gray
   "navy" ;; blue
   "gold" ;; yellow
   "maroon" ;; red
   "lavender" ;; white
   "turquoise" ;; green
   "slate gray" ;; gray
   "slate blue" ;; blue
   "moccasin" ;; yellow
   "salmon" ;; red
   "snow" ;; white
   "chartreuse" ;; green
   "honeydew" ;; gray
   "dodger blue" ;; blue
   "khaki" ;; yellow
   "violet red" ;; red
   "navajo white" ;; white
   "light sea green" ;; green
   "light slate gray" ;; gray
   "deep sky blue" ;; blue
   "light coral" ;; yellow
   ))

(defgroup phye-overlay-faces nil
  "Faces for my overlay faces."
  :group 'faces)

(defface phye-overlay-base '((t :weight bold :foreground "light green"))
  "my base face for symbol overlay highlight.")
(defface phye-highlight-face-0 '((t :inherit phye-overlay-base :background "salmon"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-1 '((t :inherit phye-overlay-base :background "slate gray"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-2 '((t :inherit phye-overlay-base :background "green"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-3 '((t :inherit phye-overlay-base :background "gray"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-4 '((t :inherit phye-overlay-base :background "blue"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-5 '((t :inherit phye-overlay-base :background "yellow"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-6 '((t :inherit phye-overlay-base :background "magenta"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-7 '((t :inherit phye-overlay-base :background "ivory"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-8 '((t :inherit phye-overlay-base :background "cyan"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-9 '((t :inherit phye-overlay-base :background "thistle"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-10 '((t :inherit phye-overlay-base :background "navy"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-11 '((t :inherit phye-overlay-base :background "gold"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-12 '((t :inherit phye-overlay-base :background "maroon"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-13 '((t :inherit phye-overlay-base :background "turquoise"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-14 '((t :inherit phye-overlay-base :background "violet red"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-15 '((t :inherit phye-overlay-base :background "chartreuse"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-16 '((t :inherit phye-overlay-base :background "light coral"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-17 '((t :inherit phye-overlay-base :background "honeydew"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-18 '((t :inherit phye-overlay-base :background "light sea green"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-19 '((t :inherit phye-overlay-base :background "light sea blue"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)

(customize-set-variable
 'symbol-overlay-faces
 '(phye-highlight-face-0
   phye-highlight-face-1
   phye-highlight-face-2
   phye-highlight-face-3
   phye-highlight-face-4
   phye-highlight-face-5
   phye-highlight-face-6
   phye-highlight-face-7
   phye-highlight-face-8
   phye-highlight-face-9
   phye-highlight-face-10
   phye-highlight-face-11
   phye-highlight-face-12
   phye-highlight-face-13
   phye-highlight-face-14
   phye-highlight-face-15
   phye-highlight-face-16
   phye-highlight-face-17
   phye-highlight-face-18
   phye-highlight-face-19
   phye-highlight-face-0
   phye-highlight-face-1
   phye-highlight-face-2
   phye-highlight-face-3
   phye-highlight-face-4
   phye-highlight-face-5
   phye-highlight-face-6
   phye-highlight-face-7
   phye-highlight-face-8
   phye-highlight-face-9
   phye-highlight-face-10
   phye-highlight-face-11
   phye-highlight-face-12
   phye-highlight-face-13
   phye-highlight-face-14
   phye-highlight-face-15
   phye-highlight-face-16
   phye-highlight-face-17
   phye-highlight-face-18
   phye-highlight-face-19))


;; customize avy jump colors
(defun recover-avy-lead-face ()
  "Recovy avy leader face."
  (interactive)
  (require 'avy)
  (set-face-attribute 'avy-lead-face nil :inherit 'font-lock-warning-face))
(with-eval-after-load 'avy
  (recover-avy-lead-face))

(unless (boundp 'font-lock-reference-face)
  (defface font-lock-reference-face '((t :inherit t :weight bold))
    "add missing font-lock-reference-face")
  (defvar font-lock-reference-face 'font-lock-reference-face))

(use-package beacon :ensure t :defer t :custom (beacon-blink-duration 0.2) :config (beacon-mode 1))

(use-package hl-anything :ensure t :defer t :config (hl-global-highlight-on/off))

(use-package ef-themes :ensure t :defer t)

(use-package doric-themes :ensure t :defer t)

(defun phye/current-hour ()
  "Return current hour."
  (let ((hour (string-to-number (format-time-string "%H" (current-time)))))
    hour))

(defvar dark-hour 17
  "Dark hour.")

(defun phye/default-theme (light)
  "Return different default theme based on frame type and whether LIGHT or not."
  (if light
      (if (display-graphic-p)
          'doric-wind
        'doom-feather-light)
    (if (display-graphic-p)
        'doom-monokai-classic
      'kaolin-galaxy)))

(defun phye/time-based-default-theme ()
  "Return default theme."
  (if (< (phye/current-hour) dark-hour)
      (phye/default-theme t)
    (phye/default-theme nil)))

(defun phye/set-ivy-match-bg-color (&optional unused)
  "Set ivy-current-match color based on current hour, UNUSED is unused."
  (interactive)
  (let ((bg-color "")
        (hour (phye/current-hour)))
    (if (< (phye/current-hour) dark-hour)
        (setq bg-color "#00FF86")
      (setq bg-color "#0065FF"))
    (set-face-background 'ivy-current-match bg-color nil)))

(defvar previous-dark-theme (phye/default-theme nil)
  "Previous dark theme before toggle.")

(defun phye/toggle-theme ()
  "Toggle theme based on current time."
  (interactive)
  (let* ((loc (getenv "LOCATION"))
         (default-theme (phye/time-based-default-theme))
         (hour (phye/current-hour))
         (light (< hour dark-hour)))
    (message "Toggle Theme at %s" loc)
    (when (equal loc "office")
      (if light
          (progn
            (setq previous-dark-theme (car custom-enabled-themes))
            (phye/load-theme default-theme))
        (phye/load-theme previous-dark-theme))
      (when (display-graphic-p)
        (shell-command
         (format "~/bin/scripts/toggle_dark_theme.sh %s"
                 (if light
                     "false"
                   "true")))))))

(advice-add 'my-random-favorite-color-theme :after #'recover-avy-lead-face)
(advice-add 'my-random-favorite-color-theme :after #'phye/set-ivy-match-bg-color)
(advice-add 'phye/load-theme :after #'phye/set-ivy-match-bg-color)

(run-at-time "09:30" 86400 #'phye/toggle-theme)
(run-at-time "17:00" 86400 #'phye/toggle-theme)

(my-run-with-idle-timer
 3
 ;; enable default theme
 (lambda ()
   (message "Load default theme...")
   (phye/toggle-theme)
   (when (display-graphic-p)
     (set-face-attribute 'default nil
                         :family "MonaspiceKr Nerd Font Mono"
                         :foundry "nil"
                         :slant 'normal
                         :weight 'regular
                         :height 130
                         :width 'normal))))

(provide 'phye-init-themes)
