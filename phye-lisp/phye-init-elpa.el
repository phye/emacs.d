;;; phye-init-elpa.el --- ELPA package list for phye's config

;;; Commentary:
;; Defines phye-elpa-packages and appends them to melpa-include-packages.

;;; Code:
(setq phye-elpa-packages
      '(activities
        acp
        shell-maker
        agent-shell
        atom-dark
        atom-one-dark
        bazel
        better-jumper
        bpftrace-mode
        bufler
        burly
        clipetty
        deadgrep
        dakrone
        dockerfile-mode
        doric-themes
        dorsey
        el-themes
        elisp-autofmt
        emacs-everywhere
        emacsql
        evil-collection
        evil-terminal-cursor-changer
        halloweenie-theme
        hemisu-theme
        hcl-mode
        helpful
        key-chord
        manoj-dark
        markdown-toc
        mermaid-mode
        multi-vterm
        nerd-icons
        noctilux
        ob-mermaid
        org-fancy-priorities
        org-journal
        org-noter
        org-present
        org-remark
        org-roam
        org-side-tree
        outline-indent
        peep-dired
        pikchr-mode
        popper
        pumpkin-spice-theme
        puni
        rg
        seti-theme
        show-font
        symbol-overlay
        sync-recentf
        tiny
        treemacs
        undo-fu-session
        wombat
        ;; vimish-fold
        zerodark))

(defvar melpa-include-packages)
(setq melpa-include-packages (append melpa-include-packages phye-elpa-packages nil))

(defvar phye/theme-packages
  '(doom-themes
    ef-themes
    doric-themes
    kaolin-themes
    gruvbox-theme
    solarized-theme
    modus-themes
    color-theme-sanityinc-tomorrow
    spacemacs-theme
    alect-themes
    atom-dark-theme
    atom-one-dark-theme
    ample-theme
    afternoon-theme
    bubbleberry-theme
    clues-theme
    cherry-blossom-theme
    cyberpunk-theme
    dakrone-theme
    darkmine-theme
    darkokai-theme
    sublime-themes
    espresso-theme
    exotica-theme
    farmhouse-themes
    gotham-theme
    grandshell-theme
    hc-zenburn-theme
    hemisu-theme
    jazz-theme
    madhat2r-theme
    noctilux-theme
    nord-theme
    occidental-theme
    omtose-phellack-themes
    planet-theme
    professional-theme
    purple-haze-theme
    rebecca-theme
    apropospriate-theme
    seti-theme
    srcery-theme
    smyx-theme
    toxi-theme
    twilight-theme
    zerodark-theme)
  "Theme packages to ensure are installed.
Built-in themes (wombat, tango-dark, manoj-dark) are intentionally omitted.")

(declare-function my-run-with-idle-timer "init-utils")

(my-run-with-idle-timer
 10
 ;; ensure all theme packages are installed
 (lambda ()
   (let (missing)
     (dolist (pkg phye/theme-packages)
       (unless (package-installed-p pkg)
         (push pkg missing)))
     (if (null missing)
         (message "phye: all theme packages already installed")
       (message "phye: installing missing theme packages: %s"
                (mapconcat #'symbol-name (reverse missing) ", "))
       (package-refresh-contents)
       (dolist (pkg missing)
         (message "Processing pkg %s" pkg)
         (condition-case err
             (package-install pkg)
           (t (message "failed to install %s – %s" pkg (error-message-string err)))))
       (message "done installing theme packages")))))

(provide 'phye-init-elpa)
;;; phye-init-elpa.el ends here
