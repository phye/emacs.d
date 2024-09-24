;; {{ latex
(use-package company-math
  :ensure t
  :defer t
  :config
  ;; (add-to-list 'company-backends 'company-math-symbols-latex)
  ;; (add-to-list 'company-backends 'company-math-symbols-unicode)
  )
;; }}

;; {{ pdf
(use-package pdf-tools
  :ensure t
  :defer 5
  :config
  (pdf-tools-install)
  (blink-cursor-mode nil)
  (add-hook 'pdf-view-mode-hook (lambda () (cua-mode 0)))
  :custom
  (pdf-view-use-scaling t)
  (pdf-annot-activate-created-annotations t)
  (pdf-view-display-size 'fit-page)
  (pdf-view-resize-factor 1.1))
;; }}

(defun phye/pdf--goto-page-by-cond (condfunc)
  "Go to title page by CONDFUNC in pdf."
  (let* ((buffer (current-buffer))
         (window (get-buffer-window buffer))
         (toc (pdf-info-outline buffer))
         (cur-page (pdf-view-current-page window))
         (found nil))
    (message "Cur-page: %d" cur-page)
    (cl-loop while (not found) do
             (let* ((title-item-0 (nth 0 toc))
                    (title-item-1 (nth 1 toc))
                    (title-page-0 (cdr (nth 3 title-item-0)))
                    (title-page-1 (cdr (nth 3 title-item-1))))
               (setq target-page (funcall condfunc cur-page title-page-0 title-page-1))
               (if (>= target-page 0)
                   (progn
                     (setq found t)
                     (pdf-view-goto-page target-page window))
                 (setq toc (cdr toc)))))))

(defun phye/pdf-goto-next-title-page ()
  "Go to next title page."
  (interactive)
  (phye/pdf--goto-page-by-cond
   (lambda (cur-page title-page-0 title-page-1)
     (if (and (<= title-page-0 cur-page) (< cur-page title-page-1))
         (progn
           ;; (message "found")
           title-page-1)
       -1))))

(defun phye/pdf-goto-prev-title-page ()
  "Go to next title page."
  (interactive)
  (phye/pdf--goto-page-by-cond
   (lambda (cur-page title-page-0 title-page-1)
     (if (and (< title-page-0 cur-page) (<= cur-page title-page-1))
         (progn
           ;; (message "found")
           title-page-0)
       -1))))

(provide 'phye-init-pdf)