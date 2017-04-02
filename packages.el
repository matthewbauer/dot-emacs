;;; packages --- List of use-package declarations.

;;; Commentary:

;;; A bunch of custom Emacs config stuff.

;;; Code:

;; Themes

;; (use-package color-theme
;;   :demand t)
;; (use-package color-theme-solarized
;;   :demand t
;;   :config
;;   (color-theme-solarized)
;;   (set-face-foreground 'secondary-selection "darkblue")
;;   (set-face-background 'secondary-selection "lightblue")
;;   (set-face-background 'font-lock-doc-face "black")
;;   (set-face-foreground 'font-lock-doc-face "wheat")
;;   (set-face-background 'font-lock-string-face "black")
;;   (set-face-foreground 'org-todo "green")
;;   (set-face-background 'org-todo "black"))

(use-package apropospriate-theme
  :demand t
  :config
  (load-theme 'apropospriate-dark))

;; key bindings

(let ((keymap-file (expand-file-name "keymap.el" user-emacs-directory)))
  (if (file-exists-p keymap-file)
      (load keymap-file)))

(use-package org)

(use-package shell)
(use-package tramp)
(use-package gnus
  :commands gnus
  :init
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode))

;; Multiple Major Modes
(use-package mmm-mode
  :commands mmm-mode)

(use-package smart-tabs-mode
  :commands smart-tabs-mode)

(use-package smartparens
  :config
  (setq smartparens-strict-mode t)
  (sp-local-pair 'emacs-lisp-mode "`" nil :when '(sp-in-string-p))
  (require 'smartparens-config)
  (sp-use-paredit-bindings)
  (smartparens-global-mode 1)
  (show-smartparens-global-mode 1)
  :bind
  (("C-M-k" . sp-kill-sexp-with-a-twist-of-lime)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-n" . sp-up-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-p" . sp-backward-down-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("M-s" . sp-splice-sexp)
   ("M-r" . sp-splice-sexp-killing-around)
   ("C-)" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("C-(" . sp-backward-slurp-sexp)
   ("C-{" . sp-backward-barf-sexp)
   ("M-S" . sp-split-sexp)
   ("M-J" . sp-join-sexp)
   ("C-M-t" . sp-transpose-sexp)))

(use-package ispell)

;; (use-package avy
;;   :config
;;   (global-set-key (kbd "C-:") 'avy-goto-char))

;; (use-package expand-region
;;   :config
;;   (global-set-key (kbd "C-=") 'expand-region))

;; Helm mode
(use-package helm
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop)
         ("C-x c SPC" . helm-all-mark-rings))
  :config
  (require 'helm-config)
  (helm-mode)
  ;; (require 'helm-eshell)
  (use-package helm-descbinds
    :config (helm-descbinds-mode))
  (use-package helm-ag)
  (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
  (define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
  ;; shell history.
  (define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
  (substitute-key-definition 'find-tag 'helm-etags-select global-map)
  (setq projectile-completion-system 'helm)
  (helm-mode 1))

(use-package lsp-mode
  :config (global-lsp-mode t))

(use-package projectile
  :commands projectile-mode
  :after helm
  :defer 5
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (use-package helm-projectile
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-toggle 1))
  (projectile-mode)
  (bind-key "s s"
            #'(lambda ()
                (interactive)
                (helm-do-grep-1 (list (projectile-project-root)) t))
            'projectile-command-map))

(use-package magithub
  :if (executable-find "git")
  :after magit
  :config (magithub-feature-autoinject t))

;; (use-package ensime)

;; (use-package framemove
;;   :config (framemove-default-keybindings 'alt))

;;
;; terminals
;;

(use-package eshell
  :bind ("C-x e" . eshell)
  :commands (eshell eshell-command)
  :preface
  (defvar eshell-isearch-map
    (let ((map (copy-keymap isearch-mode-map)))
      (define-key map [(control ?m)] 'eshell-isearch-return)
      (define-key map [return]       'eshell-isearch-return)
      (define-key map [(control ?r)] 'eshell-isearch-repeat-backward)
      (define-key map [(control ?s)] 'eshell-isearch-repeat-forward)
      (define-key map [(control ?g)] 'eshell-isearch-abort)
      (define-key map [backspace]    'eshell-isearch-delete-char)
      (define-key map [delete]       'eshell-isearch-delete-char)
      map)
    "Keymap used in isearch in Eshell.")

  :init

  (defun protect-eshell-prompt ()
    "Protect Eshell's prompt like Comint's prompts.
E.g. `evil-change-whole-line' won't wipe the prompt. This
is achieved by adding the relevant text properties."
    (let ((inhibit-field-text-motion t))
      (add-text-properties
       (point-at-bol)
       (point)
       '(rear-nonsticky t
                        inhibit-line-move-field-capture t
                        field output
                        read-only t
                        front-sticky (field inhibit-line-move-field-capture)))))

  (add-hook 'eshell-after-prompt-hook 'protect-eshell-prompt)

  (autoload 'eshell-delchar-or-maybe-eof "em-rebind")

  (defun init-eshell ()
    "Stuff to do when enabling eshell."
    (setq pcomplete-cycle-completions nil)
    (if (bound-and-true-p linum-mode) (linum-mode -1))
    (semantic-mode -1))

  (add-hook 'eshell-mode-hook 'init-eshell)

  :config

  (require 'esh-opt)

  ;; quick commands
  (defalias 'eshell/e 'find-file-other-window)
  (defalias 'eshell/d 'dired)
  (setenv "PAGER" "cat")

  ;; support `em-smart'
  (require 'em-smart)
  (add-hook 'eshell-mode-hook 'eshell-smart-initialize))

  ;; Visual commands
  (require 'em-term)
  (mapc (lambda (x) (push x eshell-visual-commands))
        '("el" "elinks" "htop" "less" "ssh" "tmux" "top"))

  ;; automatically truncate buffer after output
  (when (boundp 'eshell-output-filter-functions)
    (push 'eshell-truncate-buffer eshell-output-filter-functions))

(add-hook 'term-mode-hook 'disable-hl-line-mode)

(use-package xterm-color
  :init
  ;; Comint and Shell
  (setq comint-output-filter-functions
        (remove 'ansi-color-process-output comint-output-filter-functions))
  (defun init-eshell-xterm-color ()
    "Initialize xterm coloring for eshell"
    (setq-local xterm-color-preserve-properties t)
    (make-local-variable 'eshell-preoutput-filter-functions)
    (setq-local eshell-output-filter-functions
                (remove 'eshell-handle-ansi-color
                        eshell-output-filter-functions)))
  (add-hook 'eshell-mode-hook 'init-eshell-xterm-color))

(use-package esh-help
  :init
  (add-hook 'eshell-mode-hook 'eldoc-mode)
  (setup-esh-help-eldoc))

(use-package shell-pop
  :init
  (defmacro make-shell-pop-command (func &optional shell)
    "Create a function to open a shell via the function FUNC.
SHELL is the SHELL function to use (i.e. when FUNC represents a terminal)."
    (let* ((name (symbol-name func)))
      `(defun ,(intern (concat "shell-pop-" name)) (index)
         ,(format (concat "Toggle a popup window with `%S'.\n"
                          "Multiple shells can be opened with a numerical prefix "
                          "argument. Using the universal prefix argument will "
                          "open the shell in the current buffer instead of a "
                          "popup buffer.") func)
         (interactive "P")
         (require 'shell-pop)
         (if (equal '(4) index)
             ;; no popup
             (,func ,shell)
           (shell-pop--set-shell-type
            'shell-pop-shell-type
            (backquote (,name
                        ,(concat "*" name "*")
                        (lambda nil (,func ,shell)))))
           (shell-pop index)))))
  (make-shell-pop-command eshell)
  (make-shell-pop-command shell)
  (make-shell-pop-command term shell-pop-term-shell)
  (make-shell-pop-command multiterm)
  (make-shell-pop-command ansi-term shell-pop-term-shell))

;; (add-hook 'term-mode-hook 'ansi-term-handle-close)
(add-hook 'term-mode-hook (lambda () (linum-mode -1)))
(defun shell-comint-input-sender-hook ()
    "Check certain shell commands.
 Executes the appropriate behavior for certain commands."
    (setq comint-input-sender
          (lambda (proc command)
            (cond
             ;; Check for clear command and execute it.
             ((string-match "^[ \t]*clear[ \t]*$" command)
              (comint-send-string proc "\n")
              (erase-buffer))
             ;; Check for man command and execute it.
             ((string-match "^[ \t]*man[ \t]*" command)
              (comint-send-string proc "\n")
              (setq command (replace-regexp-in-string
                             "^[ \t]*man[ \t]*" "" command))
              (setq command (replace-regexp-in-string
                             "[ \t]+$" "" command))
              (funcall 'man command))
             ;; Send other commands to the default handler.
             (t (comint-simple-send proc command))))))
(add-hook 'shell-mode-hook 'shell-comint-input-sender-hook)

(use-package eshell-z
  :config
  (with-eval-after-load 'eshell
    (require 'eshell-z)))

(use-package eshell-prompt-extras
  :commands epe-theme-lambda
  :init
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

;;
;; IRC
;;

(use-package erc
  :defines (erc-timestamp-only-if-changed-flag
            erc-timestamp-format
            erc-fill-prefix
            erc-fill-column
            erc-insert-timestamp-function
            erc-modified-channels-alist)
  :preface
  (defun lookup-password (host user port)
    (require 'auth-source)
    (funcall (plist-get
              (car (auth-source-search
                    :host host
                    :user user
                    :type 'netrc
                    :port port))
              :secret)))

  (defun slowping (host)
    (= 0 (call-process "ping" nil nil nil "-c1" "-W5000" "-q" host)))

  (defun setup-irc-environment ()
    (setq erc-timestamp-only-if-changed-flag nil
          erc-timestamp-format "%H:%M "
          erc-fill-prefix "          "
          erc-fill-column 88
          erc-insert-timestamp-function 'erc-insert-timestamp-left)

    (defun reset-erc-track-mode ()
      (interactive)
      (setq erc-modified-channels-alist nil)
      (erc-modified-channels-update)
      (erc-modified-channels-display)
      (force-mode-line-update))

    (bind-key "C-c r" #'reset-erc-track-mode))

  (defcustom erc-foolish-content '()
    "Regular expressions to identify foolish content.
    Usually what happens is that you add the bots to
    `erc-ignore-list' and the bot commands to this list."
    :group 'erc
    :type '(repeat regexp))

  (defun erc-foolish-content (msg)
    "Check whether MSG is foolish."
    (erc-list-match erc-foolish-content msg))

  :init
  (add-hook 'erc-mode-hook 'setup-irc-environment)
  (add-to-list
   'erc-mode-hook
   #'(lambda () (set (make-local-variable 'scroll-conservatively) 100)))

  :config
  (erc-track-minor-mode 1)
  (erc-track-mode 1)

  (add-hook 'erc-insert-pre-hook
            (lambda (s)
              (when (erc-foolish-content s)
                (setq erc-insert-this nil)))
            )
      (erc-services-mode 1)
      (defun erc-list-command ()
        "execute the list command"
        (interactive)
        (insert "/list")
        (erc-send-current-line))
      (setq erc-kill-buffer-on-part t
            erc-kill-queries-on-quit t
            erc-kill-server-buffer-on-quit t)
      (add-hook 'erc-connect-pre-hook (lambda (x) (erc-update-modules)))
      (erc-track-mode t)
      (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE")
            erc-server-coding-system '(utf-8 . utf-8))
      (setq erc-prompt (lambda () (concat "[" (buffer-name) "]")))

      (require 'notifications)
      (defun erc-global-notify (match-type nick message)
        "Notify when a message is recieved."
        (notifications-notify
         :title nick
         :body message
         :urgency 'low)))

;; Flycheck mode
(use-package flycheck
  :init
  (global-flycheck-mode t)
  :config
  (setq flycheck-display-errors-function nil))

(use-package auto-dictionary
  :init
  (add-hook 'flyspell-mode-hook 'auto-dictionary-mode))

(use-package flyspell
  :commands (spell-checking/change-dictionary)
  :init
  (progn
    (add-hook 'text-mode-hook' 'flyspell-mode)
    ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
    ))

(use-package flyspell-correct
  :commands (flyspell-correct-word-generic
             flyspell-correct-previous-word-generic))

;; Company mode
(use-package company
  :bind ("<C-tab>" . company-complete)
  :commands company-mode
  :after helm
  :config (global-company-mode 1)
  (add-hook 'after-init-hook 'global-company-mode)
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

;; (use-package gist)

(use-package magit
  :if (executable-find "git")
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-dispatch-popup))
  :config
  (setq magit-revision-show-gravatars '("^Author:     " . "^Commit:
  "))
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  )

(use-package magit-gh-pulls
  :if (executable-find "git")
  :config
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

(use-package ycmd)
(use-package flycheck-ycmd
  :init (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup))
(use-package company-ycmd
  :after ycmd
  :commands company-ycmd
  :init
  (company-ycmd-setup))

(defun my:setup-imenu-for-use-package ()
  "Recognize `use-package` in imenu"
  (when (string= buffer-file-name (expand-file-name "packages.el" "~/.emacs.d"))
    (add-to-list
     'imenu-generic-expression
     '("Packages" "^\\s-*(\\(use-package\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2))))

(add-hook 'emacs-lisp-mode-hook 'my:setup-imenu-for-use-package)

(use-package imenu-list
  :init
  (setq imenu-list-focus-after-activation t
        imenu-list-auto-resize t))

;; (use-package deft)

;;
;; minor modes
;;

;; (use-package golden-ratio
;;   :config
;;   (global-golden-ratio-mode))

;; (use-package hideshow
;;   :bind ("C-c h" . hs-toggle-hiding)
;;   :commands hs-toggle-hiding)

;; (use-package ispell)

(use-package wrap-region
  :commands wrap-region-mode
  :diminish wrap-region-mode
  :config
  (wrap-region-add-wrappers
   '(("$" "$")
     ("/" "/" nil ruby-mode)
     ("/* " " */" "#" (java-mode javascript-mode css-mode c-mode c++-mode))
     ("`" "`" nil (markdown-mode ruby-mode shell-script-mode)))))

(use-package whitespace-cleanup-mode
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

;; (use-package undo-tree
;;   :config
;;   (global-undo-tree-mode t))

;; (use-package multi-term
;;   :commands multi-term
;;   :init
;;   (add-hook 'term-mode-hook
;;             (lambda () (yas-minor-mode -1))))

;; (use-package ace-jump-mode)

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

(use-package rainbow-delimiters
  :init
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

;; (use-package ace-window
;;   :bind (("C-x a" . ace-window)))

(use-package ag
  :if (executable-find "ag")
  :bind
  ("M-?" . ag-project))

(use-package page-break-lines
  :init
  (add-hook 'doc-mode-hook 'page-break-lines-mode))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'helm)

  (dumb-jump-mode))

(use-package editorconfig
  :if (executable-find "editorconfig")
  :mode ("\\.editorconfig\\'" . conf-unix-mode)
  :config
  (editorconfig-mode 1))

;; (use-package ranger
;;   :commands (ranger deer ranger-override-dired-fn)
;;   :config
;;   (define-key ranger-mode-map (kbd "-") 'ranger-up-directory))

(use-package fasd
  :config
  (global-fasd-mode 1)
  ;; we will fall back to using the default completing-read function, which is helm once helm is loaded.
  (setq fasd-completing-read-function 'nil)
  )

(use-package aggressive-indent
  :init
  (add-hook 'lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  )

;; (use-package which-key)

;; (use-package typo)

;; (use-package semantic)
;; (use-package semantic-refractor)

(use-package hungry-delete
  :config
  (setq-default hungry-delete-chars-to-skip " \t\f\v") ; only horizontal whitespace
  (define-key hungry-delete-mode-map (kbd "DEL") 'hungry-delete-backward)
  (define-key hungry-delete-mode-map (kbd "S-DEL") 'delete-backward-char)
  (global-hungry-delete-mode)
  )

;;
;; editor modes
;;

;; big editor modes

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (js2-imenu-extras-mode)
  ;; (use-package skewer-mode
  ;;   :commands skewer-mode)
  )

(use-package intero
  :config (add-hook 'haskell-mode-hook 'intero-mode))

(use-package tern
  :config
  (add-hook 'js2-mode-hook 'tern-mode))

;; (use-package jdee)

;; small editor modes

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

(use-package pandoc-mode)

;; (use-package ox-pandoc
;;   :init (with-eval-after-load 'org (require 'ox-pandoc))
;;   :if (executable-find "pandoc")
;;   )

;;
;; fun
;;

;; (use-package twittering-mode)

;; (use-package xkcd)

;; (use-package tetris)

;; (use-package pacmacs)

;; (use-package spray)

(use-package esup)

(provide 'packages)
;;; packages.el ends here
