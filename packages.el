;;; packages --- List of use-package declarations.

;;; Commentary:

;;; A bunch of custom Emacs config stuff.

;;; Code:

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
  :commands mmm-mode)

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
  :init (global-flycheck-mode))

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
  (global-undo-tree-mode t))

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
;; editor modes
;;

(use-package js2-mode
  :mode "\\.js\\'"
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

(use-package crontab-mode
  :mode "\\.?cron\\(tab\\)?\\'")

(use-package css-mode
  :commands css-mode
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

(provide 'packages)
;;; packages.el ends here
