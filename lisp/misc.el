;; default global modes
(electric-pair-mode t)
(electric-indent-mode t)
(show-paren-mode 1)
;; (winner-mode t)
;; (which-function-mode t)
;; (ido-mode -1)
(cua-selection-mode t)
;; (semantic-mode 1)
;; (desktop-save-mode t)
(blink-cursor-mode 0)
;; (icomplete-mode 1)
(save-place-mode)
(delete-selection-mode t)
(savehist-mode t)
(delete-selection-mode t)
;; (global-auto-revert-mode t)
;; (global-undo-tree-mode)
;; (global-anzu-mode)
(menu-bar-mode -1)
;; (line-number-mode t)
;; (column-number-mode t)
;; (size-indication-mode t)
(toggle-scroll-bar -1)

;; show the cursor when moving after big movements in the window
;; (require 'beacon)
;; (beacon-mode +1)

;; show available keybindings after you start typing
;; (require 'which-key)
;; (which-key-mode +1)

(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)

;; Enable emoji, and stop the UI from freezing when trying to display them.
(if (fboundp 'set-fontset-font)
    (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

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

(defadvice set-buffer-major-mode (after set-major-mode activate compile)
  "Set buffer major mode according to `auto-mode-alist'."
  (let* ((name (buffer-name buffer))
         (mode (assoc-default name auto-mode-alist 'string-match)))
    (when (and mode (consp mode))
      (setq mode (car mode)))
    (with-current-buffer buffer (if mode (funcall mode)))))

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

(defadvice exchange-point-and-mark (before deactivate-mark activate compile)
  "When called with no active region, do not activate mark."
  (interactive
   (list (not (region-active-p)))))

(defmacro with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))

(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; Compilation from Emacs
(defun colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

;; Colorize output of Compilation Mode, see
;; http://stackoverflow.com/a/3072831/355252
(add-hook 'compilation-filter-hook #'colorize-compilation-buffer)

(defadvice server-visit-files (before parse-numbers-in-lines (files proc &optional nowait) activate)
  "Open file with emacsclient with cursors positioned on requested line.
Most of console-based utilities prints filename in format
'filename:linenumber'.  So you may wish to open filename in that format.
Just call:
  emacsclient filename:linenumber
and file 'filename' will be opened and cursor set on line 'linenumber'"
  (ad-set-arg 0
              (mapcar (lambda (fn)
                        (let ((name (car fn)))
                          (if (string-match "^\\(.*?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?$" name)
                              (cons
                               (match-string 1 name)
                               (cons (string-to-number (match-string 2 name))
                                     (string-to-number (or (match-string 3 name) ""))))
                            fn))) files)))

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

;; It's all in the Meta
;; (setq ns-function-modifier 'hyper)

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

;; This is an eshell alias
(defun eshell/clear ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (eshell-send-input))

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

(defun setup-imenu-for-use-package ()
  "Recognize `use-package` in imenu"
  (when (string= buffer-file-name (expand-file-name "packages.el" "~/.emacs.d"))
    (add-to-list
     'imenu-generic-expression
     '("Packages" "^\\s-*(\\(use-package\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2))))

(add-hook 'emacs-lisp-mode-hook 'setup-imenu-for-use-package)

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

(require 'notifications)

(defun erc-global-notify (match-type nick message)
  "Notify when a message is recieved."
  (notifications-notify
   :title nick
   :body message
   :urgency 'low))

(provide 'misc)
