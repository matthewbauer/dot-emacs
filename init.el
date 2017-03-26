;;; init.el -- Initialize Emacs    -*- no-byte-compile: t -*-

;;; Commentary:

;;; A bunch of custom Emacs config stuff.

;;; Code:

(let ((file-name-handler-alist nil))

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

  ;;
  ;; Load and init package, use-package
  ;;

  (require 'package)

  (setq package-enable-at-startup nil)
  (package-initialize)

  (unless (assoc-default "melpa" package-archives)
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t))

  (unless (package-installed-p 'use-package)
    (package-install 'use-package))

  ;;(eval-and-compile
  ;;  (mapc #'(lambda (path)
  ;;            (add-to-list 'load-path
  ;;                         (expand-file-name path
  ;;                                           user-emacs-directory)))
  ;;        '("site-lisp" "override" "lisp" "lisp/use-package" ""))
  ;;  (package-initialize t)
  ;;  (require 'use-package))

  ;; auto compile
  (use-package auto-compile
    :demand t
    :config
    (auto-compile-on-load-mode))

  ;; (require 'custom)

  (let ((private-file (expand-file-name "private.el" user-emacs-directory)))
    (if (file-exists-p private-file)
        (load private-file)))

  (require 'misc)

  ;; server
  ;; (require 'server)
  ;; (unless (server-running-p)
  ;;   (server-start))

  ;; packages

  (let ((packages-file (expand-file-name "packages.el" user-emacs-directory)))
    (if (file-exists-p packages-file)
        (load packages-file)))
  )

(provide 'init)

;;; init.el ends here
