;; Settings.el

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-auto-save t)
 '(TeX-close-quote "")
 '(TeX-master nil)
 '(TeX-open-quote "")
 '(TeX-parse-self t)
 '(ad-redefinition-action (quote accept))
 '(ag-highlight-search t)
 '(apropos-do-all t)
 '(async-bytecomp-package-mode t)
 '(auru-warn-only t)
 '(auth-source-save-behavior t)
 '(auto-revert-check-vc-info t)
 '(auto-revert-use-notify nil)
 '(auto-revert-verbose nil)
 '(auto-save-file-name-transforms (\` ((".*" (\, temporary-file-directory) t))))
 '(backup-directory-alist (\` ((".*" \, temporary-file-directory))))
 '(bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory))
 '(bookmark-save-flag 1)
 '(byte-compile-verbose nil)
 '(c-eldoc-includes "" t)
 '(comint-process-echoes t)
 '(comint-prompt-read-only t)
 '(company-auto-complete (quote (quote company-explicit-action-p)))
 '(company-idle-delay 0.5)
 '(company-minimum-prefix-length 2)
 '(company-show-numbers t)
 '(company-tooltip-limit 10)
 '(compilation-always-kill t)
 '(compilation-ask-about-save nil)
 '(compilation-auto-jump-to-first-error t)
 '(compilation-scroll-output (quote first-error))
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
 '(custom-browse-order-groups (quote first))
 '(custom-buffer-done-kill t)
 '(custom-safe-themes t)
 '(default-input-method "TeX")
 '(delete-old-versions -1)
 '(desktop-lazy-verbose nil)
 '(desktop-load-locked-desktop nil)
 '(desktop-missing-file-warning nil)
 '(desktop-not-loaded-hook (quote desktop-save-mode-off))
 '(desktop-restore-eager 1)
 '(desktop-restore-frames t)
 '(dired-dwim-target t)
 '(dired-mode-hook (quote (auto-revert-mode)))
 '(dired-recursive-copies (quote always))
 '(dired-recursive-deletes (quote top))
 '(display-buffer-alist
   (\`
    (((\,
       (rx bos
           (or "*Flycheck errors*" "*Backtrace" "*Warnings" "*compilation" "*Help" "*less-css-compilation" "*Packages" "*magit-process" "*SQL" "*tldr")))
      (display-buffer-reuse-window display-buffer-in-side-window)
      (side . bottom)
      (reusable-frames . visible)
      (window-height . 0.33))
     ("." nil
      (reusable-frames . visible)))))
 '(dumb-jump-selector (quote helm))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(erc-autoaway-idle-seconds 600)
 '(erc-autoaway-use-emacs-idle t)
 '(erc-autojoin-timing (quote ident))
 '(erc-fill-column 88)
 '(erc-fill-prefix "          ")
 '(erc-insert-timestamp-function (quote erc-insert-timestamp-left))
 '(erc-interpret-mirc-color t)
 '(erc-join-buffer (quote bury))
 '(erc-kill-buffer-on-part t)
 '(erc-kill-queries-on-quit t)
 '(erc-kill-server-buffer-on-quit t)
 '(erc-modules
   (quote
    (autojoin button capab-identify completion dcc fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring services stamp track)))
 '(erc-prompt (lambda nil (concat "[" (buffer-name) "]")))
 '(erc-prompt-for-nickserv-password nil)
 '(erc-prompt-for-password nil)
 '(erc-query-display (quote buffer))
 '(erc-rename-buffers t)
 '(erc-save-buffer-on-part t)
 '(erc-server "irc.freenode.net")
 '(erc-server-coding-system (quote (utf-8 . utf-8)))
 '(erc-timestamp-format "%H:%M ")
 '(erc-timestamp-only-if-changed-flag nil)
 '(erc-track-exclude-types
   (quote
    ("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477")))
 '(erc-try-new-nick-p nil)
 '(eshell-banner-message "")
 '(eshell-buffer-maximum-lines 20000)
 '(eshell-buffer-shorthand t)
 '(eshell-cd-on-directory t)
 '(eshell-cmpl-autolist t)
 '(eshell-cmpl-cycle-completions t)
 '(eshell-cmpl-expand-before-complete t)
 '(eshell-cmpl-ignore-case t)
 '(eshell-complete-export-definition t)
 '(eshell-cp-interactive-query t)
 '(eshell-cp-overwrite-files nil)
 '(eshell-default-target-is-dot t)
 '(eshell-destroy-buffer-when-process-dies t)
 '(eshell-directory-name (expand-file-name "eshell" user-emacs-directory))
 '(eshell-highlight-prompt nil)
 '(eshell-hist-ignoredups t)
 '(eshell-history-size 10000)
 '(eshell-list-files-after-cd t)
 '(eshell-ln-interactive-query t)
 '(eshell-modules-list
   (quote
    (eshell-alias eshell-basic eshell-cmpl eshell-dirs eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt eshell-rebind eshell-script eshell-smart eshell-term eshell-tramp eshell-unix eshell-xtra)))
 '(eshell-mv-interactive-query t)
 '(eshell-output-filter-functions
   (quote
    (eshell-handle-ansi-color eshell-handle-control-codes eshell-watch-for-password-prompt eshell-truncate-buffer)))
 '(eshell-plain-echo-behavior nil)
 '(eshell-prompt-function (quote epe-theme-lambda) t)
 '(eshell-prompt-regexp "^[^λ]+ λ ")
 '(eshell-review-quick-commands t)
 '(eshell-rm-interactive-query t)
 '(eshell-send-direct-to-subprocesses nil)
 '(eshell-smart-space-goes-to-end t)
 '(eshell-visual-commands
   (quote
    ("vi" "screen" "top" "less" "more" "lynx" "ncftp" "pine" "tin" "trn" "elm" "nano")))
 '(eshell-where-to-jump (quote begin))
 '(esup-child-profile-require-level 3 t)
 '(explicit-bash-args (quote ("-c" "export EMACS=; stty echo; bash")))
 '(explicit-shell-file-name "bash")
 '(fased-completing-read-function (quote nil))
 '(flycheck-check-syntax-automatically (quote (save idle-change mode-enabled)))
 '(flycheck-global-modes (quote (not erc-mode message-mode
                                     git-commit-mode)))
 '(flycheck-idle-change-delay 0.8)
 '(flycheck-standard-error-navigation t)
 '(flyspell-abbrev-p nil)
 '(flyspell-auto-correct nil)
 '(flyspell-highlight-properties nil)
 '(flyspell-incorrect-hook nil)
 '(flyspell-issue-welcome-flag nil)
 '(flyspell-use-meta-tab nil)
 '(frame-title-format
   (quote
    ((:eval
      (if
          (buffer-file-name)
          (abbreviate-file-name
           (buffer-file-name))
        "%b")))) t)
 '(gc-cons-threshold 100000000)
 '(geiser-mode-start-repl-p t)
 '(glasses-separator "-")
 '(glasses-uncapitalize-p t)
 '(global-auto-revert-non-file-buffers t)
 '(global-magit-file-mode t)
 '(gnutls-verify-error nil)
 '(helm-autoresize-mode t)
 '(helm-buffer-skip-remote-checking t)
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
 '(helm-ff-guest-ffap-filenames t)
 '(helm-ff-no-preselect t)
 '(helm-ff-search-library-in-sexp t)
 '(helm-ff-skip-boring-files t)
 '(helm-for-files-preferred-list
   (quote
    (helm-source-files-in-current-dir helm-source-recentf helm-source-bookmarks helm-source-file-cache helm-source-buffers-list helm-source-locate helm-source-ls-git)))
 '(helm-ls-git-show-abs-or-relative (quote relative))
 '(helm-quick-update t)
 '(helm-recentf-fuzzy-match t)
 '(helm-reuse-last-window-split-state t)
 '(history-delete-duplicates t)
 '(history-length 200)
 '(ibuffer-display-summary nil)
 '(ibuffer-show-empty-filter-groups nil)
 '(ido-auto-merge-work-directories-length -1)
 '(ido-create-new-buffer (quote always))
 '(ido-default-file-method (quote selected-window))
 '(ido-enable-flex-matching t)
 '(ido-enable-prefix nil)
 '(ido-max-prospects 10)
 '(ido-use-faces nil)
 '(ido-use-filename-at-point (quote guess))
 '(iedit-toggle-key-default nil)
 '(imenu-list-auto-resize t)
 '(imenu-list-focus-after-activation t)
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(initial-major-mode (quote fundamental-mode))
 '(initial-scratch-message "")
 '(initsplit-customizations-alist
   (quote
    (("\\`\\(gnus\\|nn\\|message\\|mail\\|mm-\\|smtp\\|send-mail\\|check-mail\\|spam\\|sc-\\)"
      (concat user-emacs-directory "gnus-settings.el")
      nil nil)
     ("\\`\\(org-\\|deft-\\|cfw:\\)"
      (concat user-emacs-directory "org-settings.el")
      nil nil))))
 '(ispell-extra-args (quote ("--sug-mode=ultra")))
 '(ispell-program-name "aspell")
 '(js-indent-level 2)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-p nil)
 '(js2-mode-show-parse-errors nil)
 '(js2-mode-show-strict-warnings nil)
 '(js2-strict-missing-semi-warning nil)
 '(kill-do-not-save-duplicates t)
 '(kill-whole-line t)
 '(load-prefer-newer t)
 '(mac-mouse-wheel-mode t)
 '(magit-auto-revert-mode t)
 '(magit-clone-set-remote\.pushDefault t)
 '(magit-completing-read-function (quote magit-builtin-completing-read) t)
 '(magit-delete-by-moving-to-trash t)
 '(magit-diff-options nil)
 '(magit-ediff-dwim-show-on-hunks t)
 '(magit-fetch-arguments nil)
 '(magit-highlight-trailing-whitespace nil)
 '(magit-highlight-whitespace nil)
 '(magit-mode-hook
   (quote
    (magit-load-config-extensions magit-xref-setup bug-reference-mode turn-on-magit-gh-pulls)) t)
 '(magit-no-confirm t)
 '(magit-process-connection-type nil)
 '(magit-process-find-password-functions (quote (magit-process-password-auth-source)))
 '(magit-process-popup-time 15)
 '(magit-push-always-verify nil)
 '(magit-revision-mode-hook (quote (bug-reference-mode)))
 '(magit-revision-show-gravatars (quote ("^Author:     " . "^Commit:
  ")))
 '(magit-save-repository-buffers (quote dontask))
 '(magit-stage-all-confirm nil)
 '(magit-unstage-all-confirm nil)
 '(make-backup-files nil)
 '(makefile-electric-keys t)
 '(mmm-submode-decoration-level 0)
 '(mouse-yank-at-point t)
 '(network-security-level (quote medium))
 '(nrepl-log-messages t)
 '(nroff-electric-mode t)
 '(nsm-save-host-names t)
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/"))))
 '(package-check-signature nil)
 '(package-directory-list nil)
 '(package-load-list (quote (all)))
 '(package-selected-packages
   (quote
    (tiny mu4e elixir-mode-iex inf-ruby nodejs-repl psysh repl-toggle multi-line neotree make-it-so deft iedit ibuffer-vc transpose-frame imenu-anywhere comint alert gnus-desktop-notify ggtags hackernews reddit-mode sx company-flx company-flx-mode company-web company-try-hard company-shell company-jedi company-irony-c-headers company-irony company-ghci company-ghc company-go company-dict company-c-headers company-auctex flycheck-ghcmod company-ycm zonokai-theme zenburn-theme yari yaml-mode xterm-color wrap-region workgroups whitespace-cleanup-mode which-key web-mode use-package undo-tree try toc-org spray spotify solarized-theme smartparens smart-tabs-mode smart-mode-line slime skewer-less shell-pop seti-theme sentence-navigation scss-mode sass-mode rust-mode ripgrep rg reveal-in-osx-finder restart-emacs redshank realgud rainbow-mode rainbow-delimiters python-mode popwin php-mode pdf-tools pcomplete-extension paredit pandoc-mode page-break-lines origami org-bullets nix-mode mwim multishell multi-term monokai-theme mode-icons mmm-mode minimap mic-paren material-theme markdown-preview-mode magithub magit-gh-pulls lua-mode lsp-mode less-css-mode launchctl latex-unicode-math-mode json-mode js2-refactor intero indium imenu-list highlight-cl guru-mode golden-ratio go-eldoc github-clone gitconfig-mode gitattributes-mode git-timemachine gist flycheck-ycmd flycheck-pos-tip flatland-theme fasd fancy-narrow eyebrowse esup eshell-z eshell-prompt-extras eshell-fringe-status eshell-autojump esh-help elisp-slime-nav editorconfig easy-kill dumb-jump docbook dired-toggle dired-ranger dired+ diffview cursor-chg csv-mode css-eldoc crux crontab-mode counsel-projectile company-ycmd company-tern company-statistics company-math company-anaconda coffee-mode cmake-mode cider c-eldoc bui bug-reference-github buffer-move bookmark+ bbdb backup-each-save auto-compile apu apropospriate-theme aggressive-indent ag ace-window ace-jump-mode)))
 '(page-break-lines-modes
   (quote
    (emacs-lisp-mode compilation-mode outline-mode prog-mode haskell-mode)))
 '(parens-require-spaces t)
 '(parse-sexp-ignore-comments t)
 '(projectile-enable-caching t)
 '(projectile-mode-line " Projectile")
 '(reb-re-syntax (quote string))
 '(require-final-newline t)
 '(ring-bell-function (quote ignore))
 '(ruby-insert-encoding-magic-comment nil)
 '(rust-format-on-save t)
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
 '(save-interprogram-paste-before-kill t)
 '(save-place-file (concat user-emacs-directory "places"))
 '(savehist-additional-variables (quote (search-ring regexp-search-ring)))
 '(savehist-autosave-interval 60)
 '(savehist-file (expand-file-name "savehist" user-emacs-directory))
 '(scroll-conservatively 100000)
 '(scroll-margin 0)
 '(scroll-preserve-screen-position 1)
 '(select-enable-clipboard t)
 '(select-enable-primary t)
 '(semanticdb-default-save-directory
   (quote
    (expand-file-name "semanticdb" user-emacs-directory)))
 '(send-mail-function (quote smtpmail-send-it))
 '(sentence-end-double-space nil)
 '(sh-learn-basic-offset t)
 '(shell-completion-execonly nil)
 '(shell-input-autoexpand t)
 '(shell-pop-full-span t)
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell)))))
 '(shell-pop-term-shell shell-file-name)
 '(shell-pop-universal-key "C-t")
 '(shell-pop-window-position (quote bottom))
 '(shell-pop-window-size 30)
 '(show-trailing-whitespace t)
 '(slime-contribs (quote (slime-fancy)) t)
 '(slime-lisp-implementations
   (quote
    ((ccl
      ("ccl"))
     (clisp
      ("clisp" "-q"))
     (cmucl
      ("cmucl" "-quiet"))
     (sbcl
      ("sbcl" "--noinform")
      :coding-system utf-8-unix))) t)
 '(sml/mode-width (quote full))
 '(sml/name-width 40)
 '(sp-autoskip-closing-pair (quote always))
 '(sp-base-key-bindings (quote paredit))
 '(sp-hybrid-kill-entire-symbol nil)
 '(tab-width 2)
 '(temporary-file-directory (expand-file-name "tmp" user-emacs-directory))
 '(term-input-autoexpand t)
 '(term-input-ring-file-name t)
 '(tls-checktrust t)
 '(tramp-default-method "ssh")
 '(undo-limit 800000)
 '(undo-tree-auto-save-history t)
 '(undo-tree-mode-lighter "")
 '(undo-tree-visualizer-diff t)
 '(undo-tree-visualizer-timestamps t)
 '(uniquify-after-kill-buffer-p t)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(uniquify-ignore-buffers-re "^\\*")
 '(uniquify-separator "/")
 '(url-automatic-caching t)
 '(use-dialog-box nil)
 '(use-file-dialog nil)
 '(use-package-always-defer t)
 '(use-package-always-defer-install nil)
 '(use-package-always-ensure t)
 '(use-package-enable-imenu-support t)
 '(use-package-expand-minimally t)
 '(use-package-verbose nil)
 '(vc-allow-async-revert t)
 '(vc-command-messages t)
 '(vc-follow-symlinks t)
 '(vc-git-diff-switches (quote ("-w" "-U3")))
 '(vc-handled-backends (quote (GIT SVN CVS Bzr Hg)))
 '(vc-make-backup-files nil)
 '(view-read-only t)
 '(visible-bell nil)
 '(visible-cursor nil)
 '(whitespace-auto-cleanup t t)
 '(whitespace-line-column 80)
 '(whitespace-rescan-timer-time nil t)
 '(whitespace-silent t t)
 '(whitespace-style (quote (face tabs empty trailing lines-tail)))
 '(windmove-wrap-around t)
 '(yas-prompt-functions
   (quote
    (yas-ido-prompt yas-completing-prompt yas-no-prompt)))
 '(yas-triggers-in-field t)
 '(yas-wrap-around-region t)
 '(ycmd-global-config
   (expand-file-name ".ycm_extra_conf.py" user-emacs-directory))
 '(ycmd-server-command (quote ("ycmd"))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'settings)
;;; settings.el ends here
