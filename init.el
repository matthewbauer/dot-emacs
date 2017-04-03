;;; init.el -- Initialize Emacs    -*- no-byte-compile: t -*-

;;; Commentary:

;;; A bunch of custom Emacs config stuff.

;;; Code:

;; used to setup custom PATH
(let ((platform-file (expand-file-name "platform.el" user-emacs-directory)))
  (if (file-exists-p platform-file)
      (load platform-file)))

;; sync exec-path with environment path
(setq exec-path (split-string (getenv "PATH") ":"))

;; temporary-file-directory
(setq temporary-file-directory (expand-file-name "tmp" user-emacs-directory))

;; custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(load custom-file)

;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" user-emacs-directory))

;;
;; Load and init package, use-package
;;

(require 'package)

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))

(setq package-enable-at-startup nil)
(package-initialize 'noactivate)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; (package-initialize)
(let ((default-directory (concat user-emacs-directory "elpa/")))
  (normal-top-level-add-subdirs-to-load-path))

(eval-when-compile
  (require 'use-package))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; packages

(require 'packages)

;;(eval-and-compile
;;  (mapc #'(lambda (path)
;;            (add-to-list 'load-path
;;                         (expand-file-name path
;;                                           user-emacs-directory)))
;;        '("site-lisp" "override" "lisp" "lisp/use-package" ""))
;;  (package-initialize t)
;;  (require 'use-package))

(require 'private nil t)

(require 'misc)

;; key bindings

(require 'keymap)

;; server
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

(provide 'init)

;;; init.el ends here
