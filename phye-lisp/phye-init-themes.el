;;; phye-init-themes.el --- Theme and visual configuration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Theme loading, highlight faces, symbol overlay, and avy color setup.

;;; Code:

(defun phye/load-theme (theme)
  "Load THEME after disable custom themes."
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(customize-set-variable
 'my-favorite-color-themes
 '(afternoon
   alect-black
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
   doom-henna
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
   doom-winter-is-coming-dark-blue
   doric-dark
   doric-fire
   ;; dorsey
   ef-autumn
   ef-bio
   ef-cherie
   ef-deuteranopia-dark
   ef-elea-dark
   ef-dark
   ef-duo-dark
   ef-night
   ef-owl
   ef-rosa
   ef-winter
   exotica
   gotham
   grandshell
   gruvbox-dark-soft
   gruvbox-dark-medium
   hc-zenburn
   hemisu-dark
   jazz
   junio
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
   occidental
   omtose-darker
   organic-green
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
   toxi
   twilight
   wombat
   zerodark))

(customize-set-variable
 'my-favorite-light-color-themes
 '(apropospriate-light
   doom-ayu-light
   doom-bluloco-light
   doom-feather-light
   doom-homage-white
   doom-nord-light
   doom-oksolar-light
   doom-one-light
   doom-opera-light
   doric-cherry
   doric-coral
   doric-earth
   doric-light
   doric-wind
   doric-marble
   kaolin-valley-light
   kaolin-mono-light
   kaolin-breeze
   ef-arbutus
   ef-cyprus
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
   espresso
   modus-operandi-tritanopia
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

(defvar phye-highlight-symbol-colors-dark
  '("salmon" ;; red
    "light yellow" ;; white
    "light green" ;; green
    "light gray" ;; gray
    "dodger blue" ;; blue
    "yellow" ;; yellow
    "orchid" ;; magenta
    "ivory" ;; white
    "cyan" ;; cyan
    "thistle" ;; gray
    "cornflower blue" ;; blue
    "gold" ;; yellow
    "light pink" ;; red
    "turquoise" ;; green
    "hot pink" ;; violet red
    "chartreuse" ;; green
    "light coral" ;; red
    "honeydew" ;; gray
    "medium spring green" ;; green
    "sky blue" ;; blue
    )
  "highlight-symbol colors suitable for dark backgrounds.")

(defvar phye-highlight-symbol-colors-light
  '("tomato" ;; red
    "wheat" ;; white
    "forest green" ;; green
    "dim gray" ;; gray
    "medium blue" ;; blue
    "goldenrod" ;; yellow
    "purple" ;; magenta
    "tan" ;; white
    "teal" ;; cyan
    "plum" ;; gray
    "light blue" ;; blue
    "dark goldenrod" ;; yellow
    "dark red" ;; red
    "dark turquoise" ;; green
    "violet red" ;; violet red
    "olive drab" ;; green
    "coral" ;; red
    "pale green" ;; gray
    "sea green" ;; green
    "steel blue" ;; blue
    )
  "highlight-symbol colors suitable for light backgrounds.")

(defun phye-update-highlight-symbol-colors (&rest _)
  "Set `highlight-symbol-colors' based on the current frame background."
  (customize-set-variable
   'highlight-symbol-colors
   (if (eq (frame-parameter nil 'background-mode) 'dark)
       phye-highlight-symbol-colors-dark
     phye-highlight-symbol-colors-light)))

(add-hook 'enable-theme-functions #'phye-update-highlight-symbol-colors)
(phye-update-highlight-symbol-colors)

(defgroup phye-overlay-faces nil
  "Faces for my overlay faces."
  :group 'faces)

(defface phye-overlay-base
  '((((background dark)) :weight bold :foreground "white")
    (((background light)) :weight bold :foreground "navy"))
  "My base face for symbol overlay highlight.")
(defface phye-highlight-face-0
  '((((background dark)) :inherit phye-overlay-base :background "salmon")
    (((background light)) :inherit phye-overlay-base :background "tomato"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-1
  '((((background dark)) :inherit phye-overlay-base :background "light gray")
    (((background light)) :inherit phye-overlay-base :background "dim gray"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-2
  '((((background dark)) :inherit phye-overlay-base :background "light green")
    (((background light)) :inherit phye-overlay-base :background "forest green"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-3
  '((((background dark)) :inherit phye-overlay-base :background "thistle")
    (((background light)) :inherit phye-overlay-base :background "plum"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-4
  '((((background dark)) :inherit phye-overlay-base :background "dodger blue")
    (((background light)) :inherit phye-overlay-base :background "light blue"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-5
  '((((background dark)) :inherit phye-overlay-base :background "yellow")
    (((background light)) :inherit phye-overlay-base :background "goldenrod"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-6
  '((((background dark)) :inherit phye-overlay-base :background "orchid")
    (((background light)) :inherit phye-overlay-base :background "purple"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-7
  '((((background dark)) :inherit phye-overlay-base :background "ivory")
    (((background light)) :inherit phye-overlay-base :background "tan"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-8
  '((((background dark)) :inherit phye-overlay-base :background "cyan")
    (((background light)) :inherit phye-overlay-base :background "teal"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-9
  '((((background dark)) :inherit phye-overlay-base :background "light yellow")
    (((background light)) :inherit phye-overlay-base :background "saddle brown"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-10
  '((((background dark)) :inherit phye-overlay-base :background "cornflower blue")
    (((background light)) :inherit phye-overlay-base :background "deep sky blue"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-11
  '((((background dark)) :inherit phye-overlay-base :background "gold")
    (((background light)) :inherit phye-overlay-base :background "dark goldenrod"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-12
  '((((background dark)) :inherit phye-overlay-base :background "light pink")
    (((background light)) :inherit phye-overlay-base :background "dark red"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-13
  '((((background dark)) :inherit phye-overlay-base :background "turquoise")
    (((background light)) :inherit phye-overlay-base :background "dark turquoise"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-14
  '((((background dark)) :inherit phye-overlay-base :background "hot pink")
    (((background light)) :inherit phye-overlay-base :background "violet red"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-15
  '((((background dark)) :inherit phye-overlay-base :background "chartreuse")
    (((background light)) :inherit phye-overlay-base :background "olive drab"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-16
  '((((background dark)) :inherit phye-overlay-base :background "light coral")
    (((background light)) :inherit phye-overlay-base :background "coral"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-17
  '((((background dark)) :inherit phye-overlay-base :background "honeydew")
    (((background light)) :inherit phye-overlay-base :background "pale green"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-18
  '((((background dark)) :inherit phye-overlay-base :background "medium spring green")
    (((background light)) :inherit phye-overlay-base :background "sea green"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-19
  '((((background dark)) :inherit phye-overlay-base :background "sky blue")
    (((background light)) :inherit phye-overlay-base :background "steel blue"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-20
  '((((background dark)) :inherit phye-overlay-base :background "burlywood")
    (((background light)) :inherit phye-overlay-base :background "chocolate"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-21
  '((((background dark)) :inherit phye-overlay-base :background "medium slate blue")
    (((background light)) :inherit phye-overlay-base :background "dark slate blue"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-22
  '((((background dark)) :inherit phye-overlay-base :background "pink")
    (((background light)) :inherit phye-overlay-base :background "deep pink"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-23
  '((((background dark)) :inherit phye-overlay-base :background "medium purple")
    (((background light)) :inherit phye-overlay-base :background "indigo"))
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
   phye-highlight-face-20
   phye-highlight-face-21
   phye-highlight-face-22
   phye-highlight-face-23))

;;;###autoload
(defun phye/highlight-region (beg end &optional all)
  "Highlight the region BEG..END with an overlay.
With prefix arg ALL, also highlight every occurrence of the region's text
in the buffer."
  (interactive "r\nP")
  (unless (use-region-p)
    (user-error "No active region"))
  (let ((text (buffer-substring-no-properties beg end)))
    (if all
        (save-excursion
          (goto-char (point-min))
          (while (search-forward text nil t)
            (phye--make-highlight-overlay (match-beginning 0) (match-end 0))))
      (phye--make-highlight-overlay beg end)))
  (deactivate-mark))

(defun phye--make-highlight-overlay (beg end)
  "Create a phye highlight overlay between BEG and END."
  (let ((ov (make-overlay beg end)))
    (overlay-put ov 'face 'hi-yellow)
    (overlay-put ov 'phye-highlight t)
    ov))

;;;###autoload
(defun phye/unhighlight-region ()
  "Delete the innermost `phye/highlight-region' overlay containing point.
If the mark is active, deactivate it."
  (interactive)
  (let* ((ovs (seq-filter (lambda (o) (overlay-get o 'phye-highlight))
                          (overlays-at (point))))
         (innermost (car (sort ovs
                               (lambda (a b)
                                 (< (- (overlay-end a) (overlay-start a))
                                    (- (overlay-end b) (overlay-start b))))))))
    (unless innermost
      (user-error "No highlight at point"))
    (delete-overlay innermost)
    (deactivate-mark)))

;; customize avy jump colors
(defun recover-avy-lead-face (&optional unused)
  "Recovy avy leader face, UNUSED is ignored."
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

(defvar phye/weekday-themes
  '((0 . (ef-summer        . ef-autumn))     ; Sunday
    (1 . (ef-spring        . ef-bio))        ; Monday
    (2 . (ef-frost         . ef-winter))     ; Tuesday
    (3 . (ef-melissa-light . ef-cherie))     ; Wednesday
    (4 . (ef-duo-light     . ef-duo-dark))   ; Thursday
    (5 . (ef-cyprus        . ef-night))      ; Friday
    (6 . (ef-elea-light    . ef-elea-dark))) ; Saturday
  "Alist mapping weekday (0=Sun..6=Sat) to a (LIGHT . DARK) theme pair.")

(defun phye/current-weekday ()
  "Return current day of week as integer (0=Sunday..6=Saturday)."
  (string-to-number (format-time-string "%w")))

(defun phye/default-theme (light)
  "Return the day's theme; LIGHT non-nil selects the light variant, else dark."
  (let ((pair (cdr (assq (phye/current-weekday) phye/weekday-themes))))
    (if light (car pair) (cdr pair))))

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
        (setq bg-color "#87CEEB")
      (setq bg-color "#0065FF"))
    (set-face-background 'ivy-current-match bg-color nil)))

(defvar previous-dark-theme (phye/default-theme nil)
  "Previous dark theme before toggle.")

(defvar previous-light-theme (phye/default-theme t)
  "Previous light theme before toggle.")

(defun phye/toggle-theme ()
  "Toggle theme based on current time."
  (interactive)
  (let* ((loc (getenv "LOCATION"))
         (default-theme (phye/time-based-default-theme))
         (hour (phye/current-hour))
         (light (< hour dark-hour))
         (current (car custom-enabled-themes))
         (target (or (if light previous-light-theme previous-dark-theme) default-theme)))
    (message "Toggle Theme at %s" loc)
    (if (equal loc "office")
        (progn
          (when (and current (not (eq current target)))
            (if light
                (setq previous-light-theme current)
              (setq previous-dark-theme current)))
          (phye/load-theme target)
          (when (display-graphic-p)
            (shell-command
             (format "~/bin/scripts/toggle_dark_theme.sh %s"
                     (if light "false" "true")))))
      (phye/load-theme default-theme))))

(advice-add 'my-pickup-random-color-theme :after #'recover-avy-lead-face)
(advice-add 'my-pickup-random-color-theme :after #'phye/set-ivy-match-bg-color)
(advice-add 'counsel-load-theme :after #'phye/set-ivy-match-bg-color)
(advice-add 'phye/load-theme :after #'phye/set-ivy-match-bg-color)

(defun phye/themes--defer-init ()
  "Deferred themes setup: load default theme."
  (message "Load default theme...")
  ;; Load `hi-lock' so the `hi-yellow' face used by `phye/highlight-region'
  ;; is defined by the time the user invokes the command.
  (require 'hi-lock)
  (run-at-time "07:30" 86400 #'phye/toggle-theme)
  (run-at-time "17:00" 86400 #'phye/toggle-theme)
  (phye/toggle-theme))

(my-run-with-idle-timer 2 #'phye/themes--defer-init)

(provide 'phye-init-themes)
;;; phye-init-themes.el ends here
