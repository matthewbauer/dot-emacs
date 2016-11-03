;;; Custom.el -- Emacs settings

;;; Commentary:

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (java-imports jdee yaml-mode wrap-region whitespace-cleanup-mode wgrep-ag web-mode use-package undo-tree smooth-scrolling smex smartscan smartparens smart-mode-line skewer-mode savekill rust-mode readline-complete rainbow-mode rainbow-delimiters php-mode peep-dired pandoc-mode page-break-lines ox-gfm origami nix-mode mwim multi-term mmm-mode miniedit markdown-mode magithub magit-gh-pulls less-css-mode jsx-mode json-mode js2-refactor jasminejs-mode intero imenu-anywhere hungry-delete htmlize highlight-symbol highlight-escape-sequences helm-swoop helm-descbinds guide-key go-mode gitignore-mode gitconfig-mode gitattributes-mode gist flycheck-pos-tip f expand-region editorconfig css-eldoc crontab-mode company-tern color-theme-solarized coffee-fof cmake-mode buffer-move auto-dictionary auto-compile ag ace-window ace-jump-mode)))

 '(user-full-name "Matthew Bauer")
 '(user-mail-address "mjbauer95@gmail.com")
 '(user-initials "mjb")

 '(undo-limit 800000)
 '(undo-tree-auto-save-history t)
 '(undo-tree-history-directory-alist (quote ((".*" . "~/.backups"))))
 '(undo-tree-mode-lighter "")
 '(undo-tree-visualizer-timestamps t)

 '(warning-minimum-log-level :error)

 '(whitespace-auto-cleanup t t)
 '(whitespace-line-column 110)
 '(whitespace-rescan-timer-time nil t)
 '(whitespace-silent t t)
 '(whitespace-style (quote (face trailing lines space-before-tab empty)))

 '(inhibit-startup-screen t)

 '(use-dialog-box nil)
 '(use-file-dialog nil)
 '(indicate-empty-lines t)
 '(column-number-mode t)
 '(initial-scratch-message "")
 '(ring-bell-function 'ignore)
 '(sentence-end-double-space nil)
 '(echo-keystrokes 0.1)

 ;; backup
 '(delete-old-versions -1)
 '(version-control t)
 '(vc-make-backup-files t)

 ;; history
 '(history-length t)
 '(history-delete-duplicates t)

 ;; tabs
 '(indent-tabs-mode nil)
 '(tab-width 2)

 '(load-prefer-newer t)

 '(package-enable-at-startup nil)

 '(global-auto-revert-non-file-buffers t)
 '(auto-revert-verbose nil)

 '(use-package-verbose t)
 '(use-package-always-ensure t)
 '(use-package-always-defer t)

 '(mmm-global-mode 'buffers-with-submode-classes)
 '(mmm-submode-decoration-level 0)

 '(flycheck-check-syntax-automatically '(save idle-change mode-enabled))
 '(flycheck-idle-change-delay 0.8)
 '(flycheck-display-errors-delay 0.0)
 '(flycheck-standard-error-navigation nil)

 '(undo-tree-visualizer-timestamps t)
 '(undo-tree-visualizer-diff t)

 '(ag-highlight-search t)

 '(desktop-save t)

 '(explicit-shell-file-name "bash")
 '(explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
 '(comint-process-echoes t)

 '(js2-basic-offset 2)
 '(js2-bounce-indent-p nil)

 '(ruby-insert-encoding-magic-comment nil)

 '(css-indent-offset 2)

 '(org-log-done t)
 '(org-completion-use-ido t)
 '(org-edit-timestamp-down-means-later t)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-span 14)
 '(org-agenda-include-diary t)
 '(org-agenda-window-setup 'current-window)
 '(org-agenda-inhibit-startup t)
 '(org-fast-tag-selection-single-key 'expert)
 '(org-tags-column 80)
 '(org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
 '(org-refile-use-outline-path (quote file))
 '(org-outline-path-complete-in-steps t)
 '(org-src-fontify-natively t)

 '(git-commit-mode-hook
   (quote
    (turn-on-auto-fill flyspell-mode git-commit-save-message)) t)

 '(emacs-lisp-mode-hook
   (quote
    (turn-on-auto-fill
     (lambda nil
       (ignore-errors
         (diminish
          (quote auto-fill-function))))
     eldoc-mode
     (lambda nil
       (local-set-key
        [(meta 46)]
        (quote find-function))
       (local-set-key
        [(control 109)]
        (quote newline-and-indent))))))

 '(eshell-history-size 1000)
 '(eshell-ls-dired-initial-args (quote ("-h")))
 '(eshell-ls-exclude-regexp "~\\'")
 '(eshell-ls-initial-args "-h")
 '(eshell-modules-list
   (quote
    (eshell-alias eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-rebind eshell-script eshell-smart eshell-term eshell-unix eshell-xtra)))
 '(eshell-prompt-function
   (lambda nil
     (concat
      (abbreviate-file-name
       (eshell/pwd))
      (if
          (=
           (user-uid)
           0)
          " # " " $ "))))
 '(eshell-save-history-on-exit t)
 '(eshell-stringify-t nil)
 '(eshell-term-name "ansi")
 '(eshell-visual-commands
   (quote
    ("vi" "top" "screen" "less" "lynx" "rlogin" "telnet")))

 '(eshell-aliases-file "~/.emacs.d/eshell.aliases") ;; FIXME: don't hardcode .emacs.d
 '(eshell-cmpl-cycle-completions nil)

 '(same-window-buffer-names
   (quote
    ("*eshell*" "*shell*" "*mail*" "*inferior-lisp*" "*ielm*" "*scheme*")))

 '(mac-pass-command-to-system nil)
 '(mac-pass-control-to-system nil)
 '(mac-wheel-button-is-mouse-2 nil)
 '(magit-auto-revert-mode nil)
 '(magit-completing-read-function (quote helm--completing-read-default))
 '(magit-diff-options nil)
 '(magit-ediff-dwim-show-on-hunks t)
 '(magit-fetch-arguments nil)
 '(magit-highlight-trailing-whitespace nil)
 '(magit-highlight-whitespace nil)
 '(magit-process-popup-time 15)
 '(magit-push-always-verify nil)
 '(magit-stage-all-confirm nil)
 '(magit-unstage-all-confirm nil)
 '(magit-use-overlays nil)

 '(line-number-mode t)
 '(kill-do-not-save-duplicates t)
 '(kill-whole-line t)

 '(yas-prompt-functions
   (quote
    (yas-ido-prompt yas-completing-prompt yas-no-prompt)))
 '(yas-triggers-in-field t)
 '(yas-wrap-around-region t)

 '(vc-command-messages t)
 '(vc-follow-symlinks t)
 '(vc-git-diff-switches (quote ("-w" "-U3")))
 '(vc-handled-backends (quote (GIT SVN CVS Bzr Hg)))
 '(vc-make-backup-files t)
 '(version-control t)

 '(show-paren-delay 0)
 '(save-abbrevs (quote silently))

 '(initial-major-mode (quote fundamental-mode))

 '(history-delete-duplicates t)
 '(history-length 200)

 '(helm-ls-git-show-abs-or-relative (quote relative))
 '(helm-quick-update t)
 '(helm-recentf-fuzzy-match t)
 '(helm-delete-minibuffer-contents-from-point t)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-ff-skip-boring-files t)
 '(helm-for-files-preferred-list
   (quote
    (helm-source-files-in-current-dir helm-source-recentf helm-source-bookmarks helm-source-file-cache helm-source-buffers-list helm-source-locate helm-source-ls-git)))
 '(helm-buffers-fuzzy-matching t)
 '(helm-completing-read-handlers-alist
   (quote
    ((describe-function . helm-completing-read-symbols)
     (describe-variable . helm-completing-read-symbols)
     (debug-on-entry . helm-completing-read-symbols)
     (find-function . helm-completing-read-symbols)
     (find-tag . helm-completing-read-with-cands-in-buffer)
     (ffap-alternate-file)
     (ffap)
     (tmm-menubar)
     (find-file)
     (magit-status . ido)
     (dired-do-copy . ido)
     (dired-do-rename . ido)
     (dired-create-directory . ido)
     (mml-attach-file . ido))))

 '(flyspell-abbrev-p nil)
 '(flyspell-incorrect-hook (quote (flyspell-maybe-correct-transposition)))
 '(flyspell-use-meta-tab nil)

 '(glasses-separator "-")
 '(glasses-uncapitalize-p t)
 '(global-auto-complete-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(global-undo-tree-mode t)

 '(page-break-lines-modes
   (quote
    (emacs-lisp-mode compilation-mode outline-mode prog-mode haskell-mode)))
 '(parens-require-spaces t)
 )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-input-face ((t (:foreground "antique white"))))
 '(helm-selection ((t (:background "ForestGreen" :foreground "black"))))
 '(org-agenda-clocking ((t (:inherit secondary-selection :foreground "black"))) t)
 '(org-agenda-done ((t (:foreground "dim gray" :strike-through nil))))
 '(org-clock-overlay ((t (:background "SkyBlue4" :foreground "black"))))
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))))
 '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "cornflower blue"))))
 )

(provide 'custom)
;;; custom.el ends here
