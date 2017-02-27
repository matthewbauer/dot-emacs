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

;; temporary-file-directory
(setq temporary-file-directory (expand-file-name "tmp" user-emacs-directory))

;; custom-file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(if (file-exists-p custom-file)
    (load custom-file))

(let ((private-file (expand-file-name "private.el" user-emacs-directory)))
  (if (file-exists-p private-file)
      (load private-file)))

;; default global modes
(electric-pair-mode t)
(electric-indent-mode t)
(show-paren-mode t)
(winner-mode t)
(which-function-mode t)
(ido-mode -1)
(cua-selection-mode t)
(semantic-mode 1)
;; (desktop-save-mode t)
(auto-revert-mode t)
(blink-cursor-mode 0)
;; (icomplete-mode 1)
(save-place-mode)
(delete-selection-mode t)
(savehist-mode t)

(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

;; auto revert
(require 'autorevert)
(global-auto-revert-mode t)

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

;; temp fix for bbatsov/projectile#523
(setq projectile-mode-line " Projectile")

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

(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

(defun newline-same-column ()
  (interactive)
  (let ((col (current-column)))
    (newline)
    (indent-to col)))

(global-set-key (kbd "M-n") 'newline-same-column)

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

(provide 'init)
;;; init.el ends here
