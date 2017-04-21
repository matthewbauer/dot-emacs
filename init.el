;;; init.el -- Initialize Emacs    -*- no-byte-compile: t -*-

;;; Commentary:

;;; A bunch of custom Emacs config stuff.

;;; Code:

;; sync exec-path with environment path
(setq exec-path (split-string (getenv "PATH") ":"))

;; custom-file
(setq custom-file (expand-file-name "settings.el" user-emacs-directory))

(load custom-file)

(require 'package)

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))

(package-initialize 'noactivate)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(let ((default-directory (concat user-emacs-directory "elpa/")))
  (normal-top-level-add-to-load-path (directory-files default-directory nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))

(eval-when-compile
  (require 'use-package))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; packages

(require 'misc)

(require 'packages)

(require 'private nil t)

(require 'keymap)

;; server
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

(provide 'init)

;;; init.el ends here
