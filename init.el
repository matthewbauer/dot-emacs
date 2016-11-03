;;; init.el -- Initialize Emacs

;;; Commentary:

;;; A bunch of custom Emacs config stuff.

;;; Code:

;; used to setup custom PATH
(let ((platform-file (expand-file-name "platform.el" user-emacs-directory)))
  (if (file-exists-p platform-file)
      (load platform-file)))

;; Some initial config
(setq
 exec-path (split-string (getenv "PATH") ":")

 user-full-name "Matthew Bauer"
 user-mail-address "mjbauer95@gmail.com"

 inhibit-splash-screen t
 use-dialog-box nil
 use-file-dialog nil
 indicate-empty-lines t
 column-number-mode t
 initial-scratch-message ""
 ring-bell-function 'ignore
 sentence-end-double-space nil
 ;; tab-always-indent 'complete
 echo-keystrokes 0.1

 ;; set-mark-command-repeat-pop t

 ;; backup
 delete-old-versions -1
 version-control t
 vc-make-backup-files t

 ;; history
 history-length t
 history-delete-duplicates t

 ;; tabs
 indent-tabs-mode nil
 tab-width 2

 load-prefer-newer t

 package-enable-at-startup nil)

;; default global modes
(electric-pair-mode t)
(electric-indent-mode t)
(show-paren-mode t)
(winner-mode t)
(which-function-mode t)
(ido-mode -1)
(cua-selection-mode t)
(semantic-mode 1)
;; (blink-cursor-mode -1)
;; (icomplete-mode 1)

(setq message-log-max 16384)

(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

;; backups
(let ((backups-dir (expand-file-name "backups" user-emacs-directory)))
  (eval
   `(setq backup-directory-alist '((".*" . ,backups-dir)))))

;; auto revert
(require 'autorevert)
(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

;; Show trailing whitespace
;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                eshell-mode-hook
                eww-mode
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook))
  (add-hook hook (lambda () (setq show-trailing-whitespace nil))))

;; key bindings
(global-set-key (kbd "RET") 'newline-and-indent)

(defun newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))
(global-set-key (kbd "S-<return>") 'newline-at-end-of-line)

(windmove-default-keybindings 'meta)
(fset 'yes-or-no-p 'y-or-n-p)

;; unbind unused keys
(global-unset-key "\C-z")
(global-unset-key [?\s-p])

;; theming
(when window-system
  (custom-set-faces
   '(erc-input-face ((t (:foreground "antique white"))))
   '(helm-selection ((t (:background "ForestGreen" :foreground "black"))))
   '(org-agenda-clocking ((t (:inherit secondary-selection :foreground "black"))) t)
   '(org-agenda-done ((t (:foreground "dim gray" :strike-through nil))))
   '(org-done ((t (:foreground "PaleGreen" :weight normal :strike-through t))))
   '(org-clock-overlay ((t (:background "SkyBlue4" :foreground "black"))))
   '(org-headline-done ((((class color) (min-colors 16) (background dark)) (:foreground "LightSalmon" :strike-through t))))
   '(outline-1 ((t (:inherit font-lock-function-name-face :foreground "cornflower blue"))))))

;; don't disable narrowing
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; don't disable case change features
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; make executable after save
(add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)

;; shell mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(setenv "PAGER" "cat")
(setenv "TERM" "xterm-256color")

;;
;; org mode
;;

(require 'org)
(require 'org-agenda)
(setq org-log-done t
      org-completion-use-ido t
      org-edit-timestamp-down-means-later t
      org-agenda-start-on-weekday nil
      org-agenda-span 14
      org-agenda-include-diary t
      org-agenda-window-setup 'current-window
      org-agenda-inhibit-startup t
      org-fast-tag-selection-single-key 'expert
      org-tags-column 80
      org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))
      org-refile-use-outline-path (quote file)
      org-outline-path-complete-in-steps t
      org-src-fontify-natively t)
(add-hook 'org-mode-hook (lambda () (org-indent-mode t)))

;; org binding
(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key org-mode-map (kbd "C-M-<down>") 'org-down-element)
(define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)

;; server
(require 'server)
(unless (server-running-p)
  (server-start))

;; custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;
;; Load and init package, use-package
;;

(require 'package)

(package-initialize)

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setq
 use-package-verbose t
 use-package-always-ensure t
 use-package-always-defer t)

;; Packages

;; auto compile
(use-package auto-compile
  :demand t
  :config
  (auto-compile-on-load-mode))

;; Themes
(use-package color-theme
  :demand t)
(use-package color-theme-solarized
  :demand t
  :config
  (color-theme-solarized)
  (set-face-foreground 'secondary-selection "darkblue")
  (set-face-background 'secondary-selection "lightblue")
  (set-face-background 'font-lock-doc-face "black")
  (set-face-foreground 'font-lock-doc-face "wheat")
  (set-face-background 'font-lock-string-face "black")
  (set-face-foreground 'org-todo "green")
  (set-face-background 'org-todo "black"))

;; Multiple Major Modes
(use-package mmm-mode
  :commands mmm-mode
  :config
  (setq
   mmm-global-mode 'buffers-with-submode-classes
   mmm-submode-decoration-level 0))

;; Helm mode
(use-package helm
  :diminish helm-mode
  :init
  (require 'helm-config)
  (helm-mode)
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop)
         ("C-x c SPC" . helm-all-mark-rings)))

;; Hydra
(use-package hydra)

;; Flycheck mode
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
        flycheck-idle-change-delay 0.8
        flycheck-mode-line nil))

;; Company mode
(use-package company
  :diminish company-mode
  :bind ("<C-tab>" . company-complete)
  :init (global-company-mode 1)
  (add-hook 'after-init-hook 'global-company-mode)

  :config
  (use-package company-tern
    :init (add-to-list 'company-backends 'company-tern))

  ;; Use Helm to complete suggestions
  (define-key company-mode-map (kbd "C-:") 'helm-company)
  (define-key company-active-map (kbd "C-:") 'helm-company)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous))

;;
;; custom major modes
;;

(use-package gist)

(use-package magit
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-status-with-prefix)))

;;
;; minor modes
;;

(use-package java-imports
  :disabled t
  :config
  (add-hook 'java-mode-hook 'java-imports-scan-file))

(use-package hl-line)

(use-package hideshow
  :bind ("C-c h" . hs-toggle-hiding)
  :commands hs-toggle-hiding)

(use-package ispell)

(use-package wrap-region
  :commands wrap-region-mode
  :diminish wrap-region-mode)

(use-package whitespace-cleanup-mode
  :diminish whitespace-cleanup-mode
  :init (global-whitespace-cleanup-mode t))

(use-package buffer-move
  :commands (buf-move-up buf-move-down buf-move-left buf-move-right)
  :bind
  (("<M-S-up>" . buf-move-up)
   ("<M-S-down>" . buf-move-down)
   ("<M-S-left>" . buf-move-left)
   ("<M-S-right>" . buf-move-right)))

(use-package multiple-cursors
  :commands (mc/mark-next-like-this mc/mark-previous-like-this)
  :bind
  (("<C-S-down>" . mc/mark-next-like-this)
   ("<C-S-up>" . mc/mark-previous-like-this)))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode t)
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t))

(use-package highlight-escape-sequences
  :init
  (hes-mode))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :config
  (dolist (hook '(prog-mode-hook html-mode-hook))
    (add-hook hook 'highlight-symbol-mode)
    (add-hook hook 'highlight-symbol-nav-mode)))

(use-package multi-term
  :commands multi-term
  :init
  (add-hook 'term-mode-hook
            (lambda () (yas-minor-mode -1))))

(use-package ace-jump-mode)

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

(use-package rainbow-delimiters
  :init
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode))

(use-package ace-window
  :bind (("C-x o" . ace-window)))

(use-package ag
  :if (executable-find "ag")
  :init
  (setq-default ag-highlight-search t)
  :bind
  ("M-?" . ag-project))

(use-package savekill)

(use-package saveplace
  :config
  (save-place-mode))

(use-package page-break-lines
  :diminish page-break-lines-mode
  :init
  (global-page-break-lines-mode))

;;
;; disabled minor modes
;;

(use-package "eldoc"
  :disabled t
  :commands eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

(use-package smex
  :disabled t
  :bind ("M-x" . smex))

(use-package popwin
  :disabled t
  :config (popwin-mode t))

(use-package desktop-save
  :disabled t
  :config
  (setq desktop-save t)
  (desktop-save-mode t))

(use-package readline-complete
  :disabled t
  :config
  (setq explicit-shell-file-name "bash")
  (setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
  (setq comint-process-echoes t)
  (require 'readline-complete)
  (push 'company-readline company-backends)
  (add-hook 'rlc-no-readline-hook (lambda () (company-mode -1))))

(use-package origami
  :disabled t
  :commands origami-mode
  :config
  (add-hook 'prog-mode-hook 'origami-mode))

(use-package flyspell
  :disabled t
  :config
  (setq ispell-extra-args '("--sug-mode=ultra"
                            "--lang=uk"
                            "--run-together"
                            "--run-together-limit=5"
                            "--run-together-min=2"))
  (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)
  (if (fboundp 'prog-mode)
      (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    (dolist (hook '(lisp-mode-hook
                    emacs-lisp-mode-hook
                    scheme-mode-hook
                    clojure-mode-hook
                    ruby-mode-hook
                    yaml-mode
                    python-mode-hook
                    shell-mode-hook
                    php-mode-hook
                    css-mode-hook
                    haskell-mode-hook
                    caml-mode-hook
                    nxml-mode-hook
                    crontab-mode-hook
                    perl-mode-hook
                    tcl-mode-hook
                    javascript-mode-hook))
      (add-hook hook 'flyspell-prog-mode))))

(use-package smartparens
  :disabled t
  :diminish smartparens-mode
  :init
  (require 'smartparens-config)
  (smartparens-global-mode)
  (show-smartparens-global-mode)
  (smartparens-global-strict-mode))

(use-package smart-mode-line
  :disabled t
  :init
  (setq sml/vc-mode-show-backend t)
  (setq sml/no-confirm-load-theme t)
  :config
  (sml/setup)
  (sml/apply-theme 'automatic))

(use-package imenu-anywhere
  :disabled t
  :config
  (global-set-key (kbd "C-.") #'imenu-anywhere))

;;
;; editor modes
;;

(use-package js2-mode
  :mode "\\.js\\'"
  :init
  (setq-default
   js2-basic-offset 2
   js2-bounce-indent-p nil)
  :config
  (js2-imenu-extras-mode)
  (use-package js2-refactor)
  (use-package skewer-mode
    :commands skewer-mode))

(use-package json-mode
  :mode "\\.json\\'")

(use-package less-css-mode
  :commands less-css-mode
  :config
  (use-package js2-mode)
  (use-package skewer-less))

(use-package markdown-mode
  :mode "\\.\\(md\\|markdown\\)\\'"
  :commands markdown-mode
  :config
  (use-package pandoc-mode :init
    (add-hook 'markdown-mode-hook 'turn-on-pandoc)))

(use-package ruby-mode
  :init
  (defvar ruby-insert-encoding-magic-comment)
  (setq ruby-insert-encoding-magic-comment nil))

(use-package crontab-mode
  :mode "\\.?cron\\(tab\\)?\\'")

(use-package css-mode
  :commands css-mode
  :init
  (setq css-indent-offset 2)
  :config
  (use-package rainbow-mode
    :init
    (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
      (add-hook hook 'rainbow-mode)))
  (use-package css-eldoc))

(use-package editorconfig
  :if (executable-find "editorconfig")
  :mode ("\\.editorconfig\\'" . conf-unix-mode))

(use-package intero
  :config (add-hook 'haskell-mode-hook 'intero-mode))

(use-package tern
  :config
  (add-hook 'js2-mode-hook 'tern-mode))

(use-package nix-mode)

(use-package web-mode)

(use-package php-mode)

(use-package cmake-mode)

(use-package rust-mode)

(use-package gitattributes-mode)

(use-package gitconfig-mode)

(use-package go-mode)

(use-package coffee-mode)

(use-package gitignore-mode)

(use-package yaml-mode)

(use-package haskell-mode)

(use-package jdee)

(provide 'init)
;;; init.el ends here
