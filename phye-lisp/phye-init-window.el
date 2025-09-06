;; {{ buffer and window related
(setq split-height-threshold nil)
(setq frame-resize-pixelwise t)
(when (boundp 'fringe-mode)
  (fringe-mode 0))
(setq switch-to-buffer-in-dedicated-window 'pop)
(setq switch-to-buffer-obey-display-actions t)
(setq window-sides-slots '(1 1 1 1))

(with-eval-after-load 'ace-window
  (customize-set-variable 'aw-scope 'frame)
  (customize-set-variable 'aw-background t))

(defun phye/switch-to-previous-buffer-in-window ()
  "Switch to previous buffer in the same window."
  (interactive)
  (let* ((cur-buffer (current-buffer))
         (alt-buffer (car (car (window-prev-buffers)))))
    (if (eq cur-buffer alt-buffer)
        (switch-to-buffer (car (car (cdr (window-prev-buffers)))))
      (switch-to-buffer alt-buffer))))

(defun dedicate-current-window ()
  "Toggles window dedication in the selected window."
  (interactive)
  (set-window-dedicated-p (selected-window) (not (window-dedicated-p (selected-window))))
  (message "window dedicated: %s" (window-dedicated-p (selected-window))))

(setq xref-history-storage 'xref-window-local-history)
;; }}

(defun phye/select-next-frame ()
  "Select next frame and raise it."
  (interactive)
  (select-frame (next-frame)))

(defun phye/select-previous-frame ()
  "Select previous frame and raise it."
  (interactive)
  (select-frame (previous-frame)))

(defun phye/kill-buffer-and-frame ()
  "Kill buffer and frame."
  (interactive)
  (kill-buffer)
  (delete-frame))

(defun phye/open-recent-file-in-other-frame ()
  "Open recent file in other frame."
  (interactive)
  (let* ((files (mapcar #'substring-no-properties recentf-list))
         (hint "Recent files In Other Frame: ")
         (f (ivy-read hint files)))
    (find-file-other-frame f)))

(defun phye/resize-window-to-min ()
  "Resize window to minimum viable width."
  (interactive)
  (evil-window-set-width 80))

(defun phye/resize-window-to-max ()
  "Resize window to maximum viable width."
  (interactive)
  (evil-window-set-width 160))

(customize-set-variable 'window-min-width 40)

(defvar phye--center-window-maxmized nil
  "is center window maxmized")

(defun phye/maximize-center-window (&optional width)
  "Maximize center window by minimizing side window to WIDTH"
  (interactive "p")
  (if phye--center-window-maxmized
      (progn
        (message "Already maximized, restore...")
        (balance-windows))
    (when (< width window-min-width)
      (setq width 60))
    (message (format "Side window width: %s" width))
    (let* ((windows (window-list nil 0 (frame-first-window)))
           (center-index (/ (cl-list-length windows) 2))
           (center-window (nth center-index windows)))
      (dolist (w windows)
        (unless (eq w center-window)
          (with-selected-window w
            (evil-window-set-width width))))))
  (setq phye--center-window-maxmized (not phye--center-window-maxmized)))

(defun phye/kill-matching-buffers (regexp)
  "Kill buffers matching REGEXP without asking."
  (interactive "sKill buffers matching this regular expression: ")
  (kill-matching-buffers regexp nil t))

(require 'cl-lib)
(defun phye/kill-last-opened-buffer ()
  "Kill last opened buffer except current."
  (interactive)
  (cl-dolist
   (buf (buffer-list (selected-frame)))
   (when (and (not (eq buf (current-buffer))) (not (minibufferp buf)))
     (message "Kill buffer %s" (buffer-name buf))
     (kill-buffer buf)
     (cl-return buf))))

(defun phye/vsplit-3-and-even ()
  "Vertically split windows into 3 part."
  (interactive)
  (evil-window-vsplit)
  (evil-window-vsplit)
  (balance-windows))

(defun phye/scroll-window-left (&optional lines)
  "Scroll window left"
  (interactive "P")
  (with-selected-window (window-left (selected-window))
    (funcall (or (command-remapping #'scroll-up-command) #'scroll-up-command) lines)))

(defun phye/scroll-window-right (&optional lines)
  "Scroll window left"
  (interactive "P")
  (with-selected-window (window-right (selected-window))
    (funcall (or (command-remapping #'scroll-up-command) #'scroll-up-command) lines)))

(defun phye/xref-got-xref-and-quit ()
  (interactive)
  (xref-goto-xref t))

(defun phye/ace-move-buffer-to-window ()
  "Move buffer to another window."
  (interactive)
  (aw-select " Ace - Move buffer to window" #'phye/mv-current-buffer-to-window))

(defun phye/mv-current-buffer-to-window (window)
  "Move current buffer to WINDOW."
  (let* ((cur-buffer (current-buffer))
         (cur-window (get-buffer-window cur-buffer)))
    (with-selected-window cur-window
      (switch-to-prev-buffer cur-window))
    (set-window-buffer window cur-buffer)))

(provide 'phye-init-window)
