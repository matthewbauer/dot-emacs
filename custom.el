;;; Custom.el -- Emacs settings

;;; Commentary:

;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t t)
 '(TeX-auto-save t t)
 '(TeX-close-quote "" t)
 '(TeX-open-quote "" t)
 '(TeX-parse-self t t)
 '(ad-redefinition-action (quote accept))
 '(ag-highlight-search t)
 '(auru-warn-only t)
 '(auth-sources (quote ("~/.authinfo")))
 '(auto-revert-verbose nil)
 '(auto-save-file-name-transforms (\` ((".*" (\, temporary-file-directory) t))))
 '(backup-directory-alist (\` ((".*" \, temporary-file-directory))))
 '(column-number-mode t)
 '(comint-process-echoes t)
 '(comint-prompt-read-only t)
 '(company-idle-delay 0.5)
 '(company-minimum-prefix-length 2)
 '(company-tooltip-limit 10)
 '(cperl-clobber-lisp-bindings t)
 '(cperl-continued-statement-offset 8)
 '(cperl-electric-keywords nil)
 '(cperl-electric-lbrace-space t)
 '(cperl-electric-linefeed nil)
 '(cperl-electric-parens nil)
 '(cperl-font-lock t)
 '(cperl-indent-level 4)
 '(cperl-info-on-command-no-prompt t)
 '(cperl-invalid-face nil)
 '(cperl-lazy-help-time 3)
 '(create-lockfiles nil)
 '(css-indent-offset 2)
 '(custom-safe-themes t)
 '(delete-old-versions -1)
 '(dumb-jump-mode t)
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
 '(erc-auto-discard-away t t)
 '(erc-autoaway-idle-seconds 600 t)
 '(erc-autoaway-use-emacs-idle t t)
 '(erc-autojoin-channels-alist (quote (("freenode.net" "#nixos" "##nix-darwin"))))
 '(erc-interpret-mirc-color t)
 '(erc-join-buffer (quote bury))
 '(erc-kill-buffer-on-part t)
 '(erc-kill-queries-on-quit t)
 '(erc-kill-server-buffer-on-quit t)
 '(erc-log-channels-directory "~/.erc/logs/" t)
 '(erc-nick "matthewbauer")
 '(erc-prompt-for-password nil)
 '(erc-query-display (quote buffer))
 '(erc-save-buffer-on-part t t)
 '(erc-server-coding-system (quote (utf-8 . utf-8)))
 '(erc-spelling-mode 1)
 '(erc-track-exclude-types
   (quote
    ("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477")))
 '(erc-track-mode t)
 '(eshell-buffer-maximum-lines 20000)
 '(eshell-buffer-shorthand t)
 '(eshell-cmpl-cycle-completions nil)
 '(eshell-highlight-prompt nil)
 '(eshell-hist-ignoredups t)
 '(eshell-history-size 350)
 '(eshell-plain-echo-behavior t)
 '(eshell-review-quick-commands nil)
 '(eshell-smart-space-goes-to-end t)
 '(eshell-where-to-jump (quote begin))
 '(explicit-bash-args (quote ("-c" "export EMACS=; stty echo; bash")))
 '(explicit-shell-file-name "bash")
 '(flycheck-check-syntax-automatically (quote (save idle-change mode-enabled)))
 '(flycheck-display-errors-delay 0.0)
 '(flycheck-idle-change-delay 0.8)
 '(flycheck-standard-error-navigation nil)
 '(flyspell-abbrev-p nil)
 '(flyspell-incorrect-hook (quote (flyspell-maybe-correct-transposition)))
 '(flyspell-use-meta-tab nil)
 '(frame-title-format
   (quote
    ("" invocation-name " Prelude - "
     (:eval
      (if
          (buffer-file-name)
          (abbreviate-file-name
           (buffer-file-name))
        "%b")))) t)
 '(gc-cons-threshold 20000000)
 '(geiser-mode-start-repl-p t)
 '(git-commit-mode-hook
   (quote
    (turn-on-auto-fill flyspell-mode git-commit-save-message)) t)
 '(glasses-separator "-")
 '(glasses-uncapitalize-p t)
 '(global-auto-complete-mode t)
 '(global-auto-revert-non-file-buffers t)
 '(global-font-lock-mode t nil (font-lock))
 '(global-undo-tree-mode t)
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
 '(helm-delete-minibuffer-contents-from-point t)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-ff-skip-boring-files t)
 '(helm-for-files-preferred-list
   (quote
    (helm-source-files-in-current-dir helm-source-recentf helm-source-bookmarks helm-source-file-cache helm-source-buffers-list helm-source-locate helm-source-ls-git)))
 '(helm-ls-git-show-abs-or-relative (quote relative))
 '(helm-quick-update t)
 '(helm-recentf-fuzzy-match t)
 '(history-delete-duplicates t)
 '(history-length 200)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-create-new-buffer (quote always))
 '(ido-default-file-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(ido-enable-prefix nil)
 '(ido-max-prospects 10)
 '(ido-use-faces nil)
 '(ido-use-filename-at-point (quote guess))
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote fundamental-mode))
 '(initial-scratch-message "")
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p nil)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(js2-strict-missing-semi-warning nil)
 '(kill-do-not-save-duplicates t)
 '(kill-whole-line t)
 '(line-number-mode t)
 '(load-prefer-newer t)
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
 '(mmm-submode-decoration-level 0)
 '(mouse-wheel-progressive-speed nil)
 '(nxml-attribute-indent 4)
 '(nxml-auto-insert-xml-declaration-flag nil)
 '(nxml-bind-meta-tab-to-complete-flag t)
 '(nxml-child-indent 4)
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-include-diary t)
 '(org-agenda-inhibit-startup t)
 '(org-agenda-span 14)
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-window-setup (quote current-window))
 '(org-completion-use-ido t)
 '(org-edit-timestamp-down-means-later t)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-log-done t)
 '(org-outline-path-complete-in-steps t)
 '(org-refile-targets
   (quote
    ((nil :maxlevel . 5)
     (org-agenda-files :maxlevel . 5))))
 '(org-refile-use-outline-path (quote file))
 '(org-src-fontify-natively t)
 '(org-tags-column 80)
 '(package-enable-at-startup nil)
 '(package-selected-packages
   (quote
    (volatile-highlights highlight-parentheses aggressive-indent semantic-refractor typo-mode typo helm-ag which-key spray pacmacs xkcd rebox-mode ox-pandoc systemd-mode fasd ranger deft golden-ratio imenu-list company-ycmd flycheck-ycmd ycmd flyspell-correct erc-terminal-notifier erc-autoaway eshell-prompt-extras eshell-z shell-pop esh-help xterm-color twittering-mode dumb-jump pdf-tools tern js2-refactor d-mode auctex smartparens-ruby helm-projectile ensime smartparens-config smart-tabs-mode chess company-anaconda company-cabal company-ghc company-ghci company-go company-nixos-options company-racer company-web company-ycm js3-mode tabbar projectile lsp-mode em-unix erc-patch erc-highlight-nicknames erc-alert agda-input esh-toggle apropospriate-theme coffee-mode color-theme js2-mode multiple-cursors magit company flycheck hydra helm yaml-mode wrap-region whitespace-cleanup-mode web-mode use-package undo-tree smooth-scrolling smartscan smartparens smart-mode-line skewer-mode savekill rust-mode readline-complete rainbow-mode rainbow-delimiters php-mode peep-dired pandoc-mode page-break-lines ox-gfm origami nix-mode mwim multi-term mmm-mode miniedit markdown-mode magithub magit-gh-pulls less-css-mode jsx-mode json-mode jasminejs-mode intero imenu-anywhere hungry-delete htmlize highlight-symbol highlight-escape-sequences helm-swoop helm-descbinds guide-key go-mode gitignore-mode gitconfig-mode gitattributes-mode gist flycheck-pos-tip f expand-region editorconfig css-eldoc crontab-mode company-tern color-theme-solarized coffee-fof cmake-mode buffer-move auto-dictionary auto-compile ag ace-window ace-jump-mode)))
 '(page-break-lines-modes
   (quote
    (emacs-lisp-mode compilation-mode outline-mode prog-mode haskell-mode)))
 '(parens-require-spaces t)
 '(projectile-mode-line " Projectile")
 '(require-final-newline t)
 '(ring-bell-function (quote ignore))
 '(ruby-insert-encoding-magic-comment nil)
 '(safe-local-variable-values
   (quote
    ((eval c-set-offset
           (quote access-label)
           (quote -))
     (eval c-set-offset
           (quote substatement-open)
           0)
     (eval c-set-offset
           (quote arglist-cont-nonempty)
           (quote +))
     (eval c-set-offset
           (quote arglist-cont)
           0)
     (eval c-set-offset
           (quote arglist-intro)
           (quote +))
     (eval c-set-offset
           (quote inline-open)
           0)
     (eval c-set-offset
           (quote defun-open)
           0)
     (eval c-set-offset
           (quote innamespace)
           0)
     (indicate-empty-lines . t))))
 '(same-window-buffer-names
   (quote
    ("*eshell*" "*shell*" "*mail*" "*inferior-lisp*" "*ielm*" "*scheme*")))
 '(save-abbrevs (quote silently))
 '(scroll-conservatively 100000)
 '(scroll-margin 0)
 '(scroll-preserve-screen-position 1)
 '(semantic-mode t)
 '(send-mail-function (quote smtpmail-send-it))
 '(sentence-end-double-space nil)
 '(shell-pop-full-span t)
 '(shell-pop-term-shell shell-file-name)
 '(shell-pop-window-position (quote bottom))
 '(shell-pop-window-size 30)
 '(show-paren-delay 0)
 '(sp-autoskip-closing-pair (quote always))
 '(sp-base-key-bindings (quote paredit))
 '(sp-hybrid-kill-entire-symbol nil)
 '(tab-width 2)
 '(typo-global-mode t)
 '(undo-limit 800000)
 '(undo-tree-auto-save-history t)
 '(undo-tree-history-directory-alist (quote ((".*" . "~/.backups"))))
 '(undo-tree-mode-lighter "")
 '(undo-tree-visualizer-diff t)
 '(undo-tree-visualizer-timestamps t)
 '(uniquify-after-kill-buffer-p t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-separator "/")
 '(use-dialog-box nil)
 '(use-file-dialog nil)
 '(use-package-always-defer t)
 '(use-package-always-ensure t)
 '(user-full-name "Matthew Bauer")
 '(user-initials "mjb")
 '(user-mail-address "mjbauer95@gmail.com")
 '(vc-command-messages t)
 '(vc-follow-symlinks t)
 '(vc-git-diff-switches (quote ("-w" "-U3")))
 '(vc-handled-backends (quote (GIT SVN CVS Bzr Hg)))
 '(vc-make-backup-files t)
 '(version-control t)
 '(warning-minimum-log-level :error)
 '(whitespace-auto-cleanup t t)
 '(whitespace-line-column 110)
 '(whitespace-rescan-timer-time nil t)
 '(whitespace-silent t t)
 '(whitespace-style (quote (face trailing lines space-before-tab empty)))
 '(yas-prompt-functions
   (quote
    (yas-ido-prompt yas-completing-prompt yas-no-prompt)))
 '(yas-triggers-in-field t)
 '(yas-wrap-around-region t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-input-face ((t (:foreground "antique white"))))
 '(helm-selection ((t (:background "ForestGreen" :foreground "black"))))
 '(org-agenda-clocking ((t (:inherit secondary-selection :foreground "black"))))
 '(org-agenda-done ((t (:foreground "dim gray" :strike-through nil))))
 '(org-clock-overlay ((t (:background "SkyBlue4" :foreground "black"))))
 '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
 '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t)))))

(provide 'custom)
;;; custom.el ends here
