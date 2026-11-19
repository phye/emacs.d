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
   ample
   ample-zen
   atom-dark
   atom-one-dark
   bubbleberry
   catppuccin
   cherry-blossom
   clues
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
   doom-lantern
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
   doom-winter-is-coming-dark-blue
   doom-xcode
   doom-zenburn
   doric-dark
   doric-fire
   dorsey
   ef-autumn
   ef-bio
   ef-cherie
   ef-dark
   ef-deuteranopia-dark
   ef-duo-dark
   ef-elea-dark
   ef-night
   ef-owl
   ef-rosa
   ef-winter
   exotica
   gotham
   grandshell
   gruvbox-dark-medium
   gruvbox-dark-soft
   hc-zenburn
   hemisu-dark
   jazz
   junio
   kaolin-aurora
   kaolin-bubblegum
   kaolin-dark
   kaolin-eclipse
   kaolin-galaxy
   kaolin-ocean
   kaolin-temple
   kaolin-valley-dark
   madhat2r
   manoj-dark
   modus-vivendi
   modus-vivendi-tritanopia
   noctilux
   nord
   occidental
   omtose-darker
   organic-green
   planet
   purple-haze
   rebecca
   sanityinc-tomorrow-eighties
   sanityinc-tomorrow-night
   seti
   smyx
   solarized-gruvbox-dark
   solarized-selenized-black
   solarized-wombat-dark
   spacemacs-dark
   srcery
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
   doric-marble
   doric-wind
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
   farmhouse-light
   kaolin-breeze
   kaolin-mono-light
   kaolin-valley-light
   modus-operandi-tritanopia
   occidental
   professional
   ritchie
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
  '((((background dark)) :inherit phye-overlay-base :background "firebrick")
    (((background light)) :inherit phye-overlay-base :background "light pink"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-1
  '((((background dark)) :inherit phye-overlay-base :background "gray30")
    (((background light)) :inherit phye-overlay-base :background "gainsboro"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-2
  '((((background dark)) :inherit phye-overlay-base :background "forest green")
    (((background light)) :inherit phye-overlay-base :background "pale green"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-3
  '((((background dark)) :inherit phye-overlay-base :background "dark magenta")
    (((background light)) :inherit phye-overlay-base :background "plum"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-4
  '((((background dark)) :inherit phye-overlay-base :background "steel blue")
    (((background light)) :inherit phye-overlay-base :background "light blue"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-5
  '((((background dark)) :inherit phye-overlay-base :background "dark goldenrod")
    (((background light)) :inherit phye-overlay-base :background "khaki"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-6
  '((((background dark)) :inherit phye-overlay-base :background "dark orchid")
    (((background light)) :inherit phye-overlay-base :background "orchid"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-7
  '((((background dark)) :inherit phye-overlay-base :background "sienna")
    (((background light)) :inherit phye-overlay-base :background "wheat"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-8
  '((((background dark)) :inherit phye-overlay-base :background "teal")
    (((background light)) :inherit phye-overlay-base :background "pale turquoise"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-9
  '((((background dark)) :inherit phye-overlay-base :background "olive drab")
    (((background light)) :inherit phye-overlay-base :background "pale goldenrod"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-10
  '((((background dark)) :inherit phye-overlay-base :background "slate blue")
    (((background light)) :inherit phye-overlay-base :background "light steel blue"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-11
  '((((background dark)) :inherit phye-overlay-base :background "chocolate")
    (((background light)) :inherit phye-overlay-base :background "moccasin"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-12
  '((((background dark)) :inherit phye-overlay-base :background "medium violet red")
    (((background light)) :inherit phye-overlay-base :background "pink"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-13
  '((((background dark)) :inherit phye-overlay-base :background "cadet blue")
    (((background light)) :inherit phye-overlay-base :background "powder blue"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-14
  '((((background dark)) :inherit phye-overlay-base :background "indian red")
    (((background light)) :inherit phye-overlay-base :background "misty rose"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-15
  '((((background dark)) :inherit phye-overlay-base :background "dark olive green")
    (((background light)) :inherit phye-overlay-base :background "light green"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-16
  '((((background dark)) :inherit phye-overlay-base :background "crimson")
    (((background light)) :inherit phye-overlay-base :background "peach puff"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-17
  '((((background dark)) :inherit phye-overlay-base :background "sea green")
    (((background light)) :inherit phye-overlay-base :background "lavender"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-18
  '((((background dark)) :inherit phye-overlay-base :background "dark slate gray")
    (((background light)) :inherit phye-overlay-base :background "aquamarine"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-19
  '((((background dark)) :inherit phye-overlay-base :background "royal blue")
    (((background light)) :inherit phye-overlay-base :background "light sky blue"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-20
  '((((background dark)) :inherit phye-overlay-base :background "saddle brown")
    (((background light)) :inherit phye-overlay-base :background "burlywood"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-21
  '((((background dark)) :inherit phye-overlay-base :background "dark slate blue")
    (((background light)) :inherit phye-overlay-base :background "thistle"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-22
  '((((background dark)) :inherit phye-overlay-base :background "pale violet red")
    (((background light)) :inherit phye-overlay-base :background "lavender blush"))
  "A custom highlight face for symbol-overlay."
  :group 'phye-overlay-faces)
(defface phye-highlight-face-23
  '((((background dark)) :inherit phye-overlay-base :background "indigo")
    (((background light)) :inherit phye-overlay-base :background "medium purple"))
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
  (let* ((ovs (seq-filter (lambda (o) (overlay-get o 'phye-highlight)) (overlays-at (point))))
         (innermost
          (car
           (sort ovs
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
  '((0 . (ef-summer . ef-autumn)) ; Sunday
    (1 . (spacemacs-light . ef-bio)) ; Monday
    (2 . (ef-frost . ef-winter)) ; Tuesday
    (3 . (ef-kassio . ef-cherie)) ; Wednesday
    (4 . (ef-duo-light . ef-duo-dark)) ; Thursday
    (5 . (doom-feather-light . ef-night)) ; Friday
    (6 . (ef-elea-light . ef-elea-dark))) ; Saturday
  "Alist mapping weekday (0=Sun..6=Sat) to a (LIGHT . DARK) theme pair.")

(defun phye/current-weekday ()
  "Return current day of week as integer (0=Sunday..6=Saturday)."
  (string-to-number (format-time-string "%w")))

(defun phye/default-theme (light)
  "Return the day's theme; LIGHT non-nil selects the light variant, else dark."
  (let ((pair (cdr (assq (phye/current-weekday) phye/weekday-themes))))
    (if light
        (car pair)
      (cdr pair))))

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

(defvar previous-dark-theme nil
  "Previous dark theme before toggle.")

(defvar previous-light-theme nil
  "Previous light theme before toggle.")

(defvar phye/theme-mode nil
  "Last applied mode: \\='light or \\='dark.")

(defun phye/toggle-theme (&optional update-default)
  "Switch to the time-appropriate theme, saving the current theme.
With \\[universal-argument], also update the weekday default for the outgoing mode."
  (interactive "P")
  (let* ((loc (getenv "LOCATION"))
         (light (< (phye/current-hour) dark-hour))
         (new-mode
          (if light
              'light
            'dark))
         (current (car custom-enabled-themes)))
    (message "Toggle Theme at %s" loc)
    (if (not (equal loc "office"))
        (phye/load-theme (phye/time-based-default-theme))
      (when (and phye/theme-mode (not (eq phye/theme-mode new-mode)) current)
        (when update-default
          (let ((pair (assq (phye/current-weekday) phye/weekday-themes)))
            (when pair
              (if light
                  (setcdr (cdr pair) current)
                (setcar (cdr pair) current)))))
        (if light
            (setq previous-dark-theme current)
          (setq previous-light-theme current)))
      (setq phye/theme-mode new-mode)
      (phye/load-theme
       (if update-default
           (phye/default-theme light)
         (or (if light
                 previous-light-theme
               previous-dark-theme)
             (phye/default-theme light))))
      (when (display-graphic-p)
        (shell-command
         (format "~/bin/scripts/toggle_dark_theme.sh %s"
                 (if light
                     "false"
                   "true")))))))

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
  (setq previous-light-theme (phye/default-theme t))
  (setq previous-dark-theme (phye/default-theme nil))
  (run-at-time "07:30" 86400 #'phye/toggle-theme)
  (run-at-time "17:00" 86400 #'phye/toggle-theme)
  (phye/toggle-theme))

(my-run-with-idle-timer 2 #'phye/themes--defer-init)


(use-package mindre-theme
    :ensure t
    :vc (:url "https://github.com/erikbackman/mindre-theme" :rev newest)
    :custom
    (mindre-use-more-bold nil)
    (mindre-use-faded-lisp-parens t)
    :config
    (load-theme 'mindre t))

(provide 'phye-init-themes)
;;; phye-init-themes.el ends here
