;;; init.el -- Initialize Emacs

;;; Commentary:

;;; A bunch of custom Emacs config stuff.

;;; Code:

;; used to setup custom PATH
(let ((platform-file (expand-file-name "platform.el" user-emacs-directory)))
  (if (file-exists-p platform-file)
      (load platform-file)))

;; sync exec-path with environment path
(setq exec-path (split-string (getenv "PATH") ":"))

;; custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

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

(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

;; auto revert
(require 'autorevert)
(global-auto-revert-mode)

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

;; unbind unused keys
(global-unset-key "\C-z")
(global-unset-key [?\s-p])

(windmove-default-keybindings 'meta)
(fset 'yes-or-no-p 'y-or-n-p)

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

;; eshell mode
;; (setenv "JAVA_HOME" "/usr/local/java")
(setenv "EDITOR" "emacsclient")
(setenv "LC_ALL" "C")
(setenv "LANG" "en")

(defun eshell/emacs (&rest args)
  "Open a file in Emacs.  Some habits die hard.
ARGS unused"
  (if (null args)
      ;; If I just ran "emacs", I probably expect to be launching
      ;; Emacs, which is rather silly since I'm already in Emacs.
      ;; So just pretend to do what I ask.
      (bury-buffer)
    ;; We have to expand the file names or else weird stuff happens
    ;; when you try to open a bunch of different files in wildly
    ;; different places in the filesystem.
    (mapc #'find-file (mapcar #'expand-file-name args))))

(defun eshell/vi (&rest args)
  "Invoke `find-file' on the file.
\"vi +42 foo\" also goes to line 42 in the buffer.
ARGS unused"
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (find-file file)
          (forward-line line))
      (find-file (pop args)))))

(defun eshell-ls-find-file-at-point (point)
  "RET on Eshell's `ls' output to open files.
POINT ?"
  (interactive "d")
  (find-file (buffer-substring-no-properties
              (previous-single-property-change point 'help-echo)
              (next-single-property-change point 'help-echo))))

(defun eshell-ls-find-file-at-mouse-click (event)
  "Middle click on Eshell's `ls' output to open files.
From Patrick Anderson via the wiki.
EVENT ?"
  (interactive "e")
  (eshell-ls-find-file-at-point (posn-point (event-end event))))

;; Stolen from http://www.emacswiki.org/cgi-bin/wiki.pl/EshellEnhancedLS
(eval-after-load "em-ls"
  '(progn
     (let ((map (make-sparse-keymap)))
       (define-key map (kbd "RET")      'eshell-ls-find-file-at-point)
       (define-key map (kbd "<return>") 'eshell-ls-find-file-at-point)
       (define-key map (kbd "<mouse-2>") 'eshell-ls-find-file-at-mouse-click)
       (defvar eshell-ls-keymap map))

     (defadvice eshell-ls-decorated-name (after electrify-ls activate)
       "Eshell's `ls' now lets you click or RET on file names to open them."
       (add-text-properties 0 (length ad-return-value)
                            (list 'help-echo "RET, mouse-2: visit this file"
                                  'mouse-face 'highlight
                                  'keymap eshell-ls-keymap)
                            ad-return-value)
       ad-return-value)))

;; server
(require 'server)
(unless (server-running-p)
  (server-start))

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

;; packages

(let ((packages-file (expand-file-name "packages.el" user-emacs-directory)))
  (if (file-exists-p packages-file)
      (load packages-file)))

;; theme

(let ((theme-file (expand-file-name "theme.el" user-emacs-directory)))
  (if (file-exists-p theme-file)
      (load theme-file)))

;; key bindings

(use-package bind-key
             :demand t)
(let ((keymap-file (expand-file-name "keymap.el" user-emacs-directory)))
  (if (file-exists-p keymap-file)
      (load keymap-file)))

(provide 'init)
;;; init.el ends here
