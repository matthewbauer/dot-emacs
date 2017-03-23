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
(delete-selection-mode t)
(global-auto-revert-mode t)
;; (global-undo-tree-mode)
;; (global-anzu-mode)
;; (menu-bar-mode +1)
;; (line-number-mode t)
;; (column-number-mode t)
;; (size-indication-mode t)

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

;; saveplace remembers your location in a file when saving files
(setq save-place-file (expand-file-name "saveplace" "~/.emacs.d"))
;; activate it for all buffers
(if (< emacs-major-version 25)
    (progn (require 'saveplace)
           (setq-default save-place t))
  (save-place-mode 1))

;; savehist keeps track of some history
(require 'savehist)
(setq savehist-additional-variables
      ;; search entries
      '(search-ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (expand-file-name "savehist" "~/.emacs.d"))
(savehist-mode +1)

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

;; meaningful names for buffers with the same name
(require 'uniquify)

;; save recent files
(require 'recentf)
(setq recentf-save-file (expand-file-name "recentf" "~/.emacs.d")
      recentf-max-saved-items 500
      recentf-max-menu-items 15
      ;; disable recentf-cleanup on Emacs start, because it can cause
      ;; problems with remote files
      recentf-auto-cleanup 'never)

(defun prelude-recentf-exclude-p (file)
  "A predicate to decide whether to exclude FILE from recentf."
  (let ((file-dir (file-truename (file-name-directory file))))
    (-any-p (lambda (dir)
              (string-prefix-p dir file-dir))
            (mapcar 'file-truename (list "~/.emacs.d" package-user-dir)))))

(add-to-list 'recentf-exclude 'prelude-recentf-exclude-p)

(recentf-mode +1)

(defadvice set-buffer-major-mode (after set-major-mode activate compile)
  "Set buffer major mode according to `auto-mode-alist'."
  (let* ((name (buffer-name buffer))
         (mode (assoc-default name auto-mode-alist 'string-match)))
    (when (and mode (consp mode))
      (setq mode (car mode)))
    (with-current-buffer buffer (if mode (funcall mode)))))

;; (require 'volatile-highlights)
;; (volatile-highlights-mode t)

;; note - this should be after volatile-highlights is required
;; add the ability to cut the current line, without marking it
(require 'rect)
;; (crux-with-region-or-line kill-region)

;; tramp, for sudo access
(require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
(setq tramp-default-method "ssh")

(set-default 'imenu-auto-rescan t)

;; flyspell-mode does spell-checking on the fly as you type
(require 'flyspell)
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))

(defun prelude-cleanup-maybe ()
  "Invoke `whitespace-cleanup' if `prelude-clean-whitespace-on-save' is not nil."
  (when prelude-clean-whitespace-on-save
    (whitespace-cleanup)))

(defun prelude-enable-whitespace ()
  "Enable `whitespace-mode' if `prelude-whitespace' is not nil."
  (when prelude-whitespace
    ;; keep the whitespace decent all the time (in this buffer)
    (add-hook 'before-save-hook 'prelude-cleanup-maybe nil t)
    (whitespace-mode +1)))

(add-hook 'text-mode-hook 'prelude-enable-whitespace)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; enable erase-buffer command
(put 'erase-buffer 'disabled nil)

;; (require 'expand-region)

;; bookmarks
(require 'bookmark)
(setq bookmark-default-file (expand-file-name "bookmarks" "~/.emacs.d")
      bookmark-save-flag 1)

;; avy allows us to effectively navigate to visible things
;; (require 'avy)
;; (setq avy-background t)
;; (setq avy-style 'at-full)

;; (global-set-key (kbd "M-%") 'anzu-query-replace)
;; (global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

;; enable some really cool extensions like C-x C-j(dired-jump)
(require 'dired-x)

;; ediff - don't start another frame
(require 'ediff)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; clean up obsolete buffers automatically
(require 'midnight)

;; smarter kill-ring navigation
;; (require 'browse-kill-ring)
;; (browse-kill-ring-default-keybindings)
;; (global-set-key (kbd "s-y") 'browse-kill-ring)

(defadvice exchange-point-and-mark (before deactivate-mark activate compile)
  "When called with no active region, do not activate mark."
  (interactive
   (list (not (region-active-p)))))

(require 'tabify)
(defmacro with-region-or-buffer (func)
  "When called with no active region, call FUNC on current buffer."
  `(defadvice ,func (before with-region-or-buffer activate compile)
     (interactive
      (if mark-active
          (list (region-beginning) (region-end))
        (list (point-min) (point-max))))))

(with-region-or-buffer indent-region)
(with-region-or-buffer untabify)

;; abbrev config
(add-hook 'text-mode-hook 'abbrev-mode)

;; make a shell script executable automatically on save
(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

;; .zsh file is shell script too
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))

;; whitespace-mode config
(require 'whitespace)
(setq whitespace-line-column 80) ;; limit line length
(setq whitespace-style '(face tabs empty trailing lines-tail))

;; saner regex syntax
(require 're-builder)
(setq reb-re-syntax 'string)

(require 'eshell)
(setq eshell-directory-name (expand-file-name "eshell" "~/.emacs.d"))

(setq semanticdb-default-save-directory
      (expand-file-name "semanticdb" "~/.emacs.d"))

;; Compilation from Emacs
(defun prelude-colorize-compilation-buffer ()
  "Colorize a compilation mode buffer."
  (interactive)
  ;; we don't want to mess with child modes such as grep-mode, ack, ag, etc
  (when (eq major-mode 'compilation-mode)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max)))))

(require 'compile)
(setq compilation-ask-about-save nil  ; Just save before compiling
      compilation-always-kill t       ; Just kill old compile processes before
                                        ; starting the new one
      compilation-scroll-output 'first-error ; Automatically scroll to first
                                        ; error
      )

;; Colorize output of Compilation Mode, see
;; http://stackoverflow.com/a/3072831/355252
(require 'ansi-color)
(add-hook 'compilation-filter-hook #'prelude-colorize-compilation-buffer)

;; diff-hl
;; (global-diff-hl-mode +1)
;; (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
;; (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; easy-kill
(global-set-key [remap kill-ring-save] 'easy-kill)
(global-set-key [remap mark-sexp] 'easy-mark)

;; operate-on-number
;; (require 'operate-on-number)
;; (require 'smartrep)

;; (smartrep-define-key global-map "C-c ."
;;   '(("+" . apply-operation-to-number-at-point)
;;     ("-" . apply-operation-to-number-at-point)
;;     ("*" . apply-operation-to-number-at-point)
;;     ("/" . apply-operation-to-number-at-point)
;;     ("\\" . apply-operation-to-number-at-point)
;;     ("^" . apply-operation-to-number-at-point)
;;     ("<" . apply-operation-to-number-at-point)
;;     (">" . apply-operation-to-number-at-point)
;;     ("#" . apply-operation-to-number-at-point)
;;     ("%" . apply-operation-to-number-at-point)
;;     ("'" . operate-on-number-at-point)))

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

(defun prelude-swap-meta-and-super ()
  "Swap the mapping of Meta and Super.
Very useful for people using their Mac with a
Windows external keyboard from time to time."
  (interactive)
  (if (eq mac-command-modifier 'super)
      (progn
        (setq mac-command-modifier 'meta)
        (setq mac-option-modifier 'super)
        (message "Command is now bound to META and Option is bound to SUPER."))
    (progn
      (setq mac-command-modifier 'super)
      (setq mac-option-modifier 'meta)
      (message "Command is now bound to SUPER and Option is bound to META."))))

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

;;
;; prelude stuff
;;

(defun c-mode-common-defaults ()
  (setq c-default-style "k&r"
        c-basic-offset 4)
  (c-set-offset 'substatement-open 0))

;; this will affect all modes derived from cc-mode, like
;; java-mode, php-mode, etc
(add-hook 'c-mode-common-hook (lambda ()
                                (run-hooks 'c-mode-common-defaults)))

(defun makefile-mode-defaults ()
  (whitespace-toggle-options '(tabs))
  (setq indent-tabs-mode t ))

(add-hook 'makefile-mode-hook (lambda ()
                                (run-hooks 'makefile-mode-defaults)))

;; (require 'closure-mode)
;; (require 'cider)

(eval-after-load 'clojure-mode
  '(progn
     (defun prelude-clojure-mode-defaults ()
       (subword-mode +1)
       (run-hooks 'prelude-lisp-coding-hook))

     (setq prelude-clojure-mode-hook 'prelude-clojure-mode-defaults)

     (add-hook 'clojure-mode-hook (lambda ()
                                    (run-hooks 'prelude-clojure-mode-hook)))))

(eval-after-load 'cider
  '(progn
     (setq nrepl-log-messages t)

     (add-hook 'cider-mode-hook 'eldoc-mode)

     (defun prelude-cider-repl-mode-defaults ()
       (subword-mode +1)
       (run-hooks 'prelude-interactive-lisp-coding-hook))

     (setq prelude-cider-repl-mode-hook 'prelude-cider-repl-mode-defaults)

     (add-hook 'cider-repl-mode-hook (lambda ()
                                       (run-hooks 'prelude-cider-repl-mode-hook)))))

;; the SBCL configuration file is in Common Lisp
(add-to-list 'auto-mode-alist '("\\.sbclrc\\'" . lisp-mode))

;; Open files with .cl extension in lisp-mode
(add-to-list 'auto-mode-alist '("\\.cl\\'" . lisp-mode))

;; a list of alternative Common Lisp implementations that can be
;; used with SLIME. Note that their presence render
;; inferior-lisp-program useless. This variable holds a list of
;; programs and if you invoke SLIME with a negative prefix
;; argument, M-- M-x slime, you can select a program from that list.
(setq slime-lisp-implementations
      '((ccl ("ccl"))
        (clisp ("clisp" "-q"))
        (cmucl ("cmucl" "-quiet"))
        (sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))

;; select the default value from slime-lisp-implementations
(if (and (eq system-type 'darwin)
         (executable-find "ccl"))
    ;; default to Clozure CL on OS X
    (setq slime-default-lisp 'ccl)
  ;; default to SBCL on Linux and Windows
  (setq slime-default-lisp 'sbcl))

;; Add fancy slime contribs
(setq slime-contribs '(slime-fancy))

(add-hook 'lisp-mode-hook (lambda () (run-hooks 'prelude-lisp-coding-hook)))
;; rainbow-delimeters messes up colors in slime-repl, and doesn't seem to work
;; anyway, so we won't use prelude-lisp-coding-defaults.
(add-hook 'slime-repl-mode-hook (lambda ()
                                  (smartparens-strict-mode +1)
                                  (whitespace-mode -1)))

(eval-after-load "slime"
  '(progn
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
           slime-fuzzy-completion-in-place t
           slime-enable-evaluate-in-emacs t
           slime-autodoc-use-multiline-p t
           slime-auto-start 'always)

     (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
     (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector)))

(provide 'prelude-common-lisp)

(defun prelude-recompile-elc-on-save ()
  "Recompile your elc when saving an elisp file."
  (add-hook 'after-save-hook
            (lambda ()
              (when (and
                     (string-prefix-p prelude-dir (file-truename buffer-file-name))
                     (file-exists-p (byte-compile-dest-file buffer-file-name)))
                (emacs-lisp-byte-compile)))
            nil
            t))

(defun prelude-visit-ielm ()
  "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
  (interactive)
  (crux-start-or-switch-to 'ielm "*ielm*"))

(define-key emacs-lisp-mode-map (kbd "C-c C-z") 'prelude-visit-ielm)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-defun)
(define-key emacs-lisp-mode-map (kbd "C-c C-b") 'eval-buffer)

(defun prelude-conditional-emacs-lisp-checker ()
  "Don't check doc style in Emacs Lisp test files."
  (let ((file-name (buffer-file-name)))
    (when (and file-name (string-match-p ".*-tests?\\.el\\'" file-name))
      (setq-local flycheck-checkers '(emacs-lisp)))))

(defun prelude-emacs-lisp-mode-defaults ()
  "Sensible defaults for `emacs-lisp-mode'."
  (run-hooks 'prelude-lisp-coding-hook)
  (eldoc-mode +1)
  (prelude-recompile-elc-on-save)
  (rainbow-mode +1)
  (setq mode-name "EL")
  (prelude-conditional-emacs-lisp-checker))

(setq prelude-emacs-lisp-mode-hook 'prelude-emacs-lisp-mode-defaults)

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (run-hooks 'prelude-emacs-lisp-mode-hook)))

(add-to-list 'auto-mode-alist '("Cask\\'" . emacs-lisp-mode))

;; ielm is an interactive Emacs Lisp shell
(defun prelude-ielm-mode-defaults ()
  "Sensible defaults for `ielm'."
  (run-hooks 'prelude-interactive-lisp-coding-hook)
  (eldoc-mode +1))

(setq prelude-ielm-mode-hook 'prelude-ielm-mode-defaults)

(add-hook 'ielm-mode-hook (lambda ()
                            (run-hooks 'prelude-ielm-mode-hook)))

(eval-after-load "ielm"
  '(progn
     (define-key ielm-map (kbd "M-(") (prelude-wrap-with "("))
     (define-key ielm-map (kbd "M-\"") (prelude-wrap-with "\""))))

;; enable elisp-slime-nav-mode
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))

(defun conditionally-enable-smartparens-mode ()
  "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (smartparens-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)

;; invert the navigation direction if the the completion popup-isearch-match
;; is displayed on top (happens near the bottom of windows)
(setq company-tooltip-flip-when-above t)
(eval-after-load 'css-mode
  '(progn
     (setq css-indent-offset 2)

     (defun prelude-css-mode-defaults ()
       (rainbow-mode +1)
       (run-hooks 'prelude-prog-mode-hook))

     (setq prelude-css-mode-hook 'prelude-css-mode-defaults)

     (add-hook 'css-mode-hook (lambda ()
                                (run-hooks 'prelude-css-mode-hook)))))

;; exclude boring stuff from tracking
(erc-track-mode t)

(if (not (file-exists-p erc-log-channels-directory))
    (mkdir erc-log-channels-directory t))

;; FIXME - this advice is wrong and is causing problems on Emacs exit
;; (defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
;;   (save-some-buffers t (lambda () (when (eq major-mode 'erc-mode) t))))

;; truncate long irc buffers
(erc-truncate-mode +1)

;; enable spell checking
(erc-spelling-mode 1)

(defvar erc-notify-nick-alist nil
  "Alist of nicks and the last time they tried to trigger a
notification")

(defvar erc-notify-timeout 10
  "Number of seconds that must elapse between notifications from
the same person.")

(defun erc-notify-allowed-p (nick &optional delay)
  "Return non-nil if a notification should be made for NICK.
If DELAY is specified, it will be the minimum time in seconds
that can occur between two notifications.  The default is
`erc-notify-timeout'."
  (unless delay (setq delay erc-notify-timeout))
  (let ((cur-time (time-to-seconds (current-time)))
        (cur-assoc (assoc nick erc-notify-nick-alist))
        (last-time nil))
    (if cur-assoc
        (progn
          (setq last-time (cdr cur-assoc))
          (setcdr cur-assoc cur-time)
          (> (abs (- cur-time last-time)) delay))
      (push (cons nick cur-time) erc-notify-nick-alist)
      t)))

(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "Do you want to start IRC? ")
    (erc :server "irc.freenode.net" :port 6667 :nick erc-nick)))

(defun filter-server-buffers ()
  (delq nil
        (mapcar
         (lambda (x) (and (erc-server-buffer-p x) x))
         (buffer-list))))

(defun stop-irc ()
  "Disconnects from all irc servers"
  (interactive)
  (dolist (buffer (filter-server-buffers))
    (message "Server buffer: %s" (buffer-name buffer))
    (with-current-buffer buffer
      (erc-quit-server "Asta la vista"))))

;; (require 'go-projectile)

;; Ignore go test -c output files
(add-to-list 'completion-ignored-extensions ".test")

(define-key 'help-command (kbd "G") 'godoc)

(eval-after-load 'go-mode
  '(progn
     (defun prelude-go-mode-defaults ()
       ;; Add to default go-mode key bindings
       (let ((map go-mode-map))
         (define-key map (kbd "C-c a") 'go-test-current-project) ;; current package, really
         (define-key map (kbd "C-c m") 'go-test-current-file)
         (define-key map (kbd "C-c .") 'go-test-current-test)
         (define-key map (kbd "C-c b") 'go-run)
         (define-key map (kbd "C-h f") 'godoc-at-point))

       ;; Prefer goimports to gofmt if installed
       (let ((goimports (executable-find "goimports")))
         (when goimports
           (setq gofmt-command goimports)))

       ;; gofmt on save
       (add-hook 'before-save-hook 'gofmt-before-save nil t)

       ;; stop whitespace being highlighted
       (whitespace-toggle-options '(tabs))

       ;; Company mode settings
       (set (make-local-variable 'company-backends) '(company-go))

       ;; El-doc for Go
       (go-eldoc-setup)

       ;; CamelCase aware editing operations
       (subword-mode +1))

     (setq prelude-go-mode-hook 'prelude-go-mode-defaults)

     (add-hook 'go-mode-hook (lambda ()
                               (run-hooks 'prelude-go-mode-hook)))))

(eval-after-load 'haskell-mode
  '(progn
     (defun prelude-haskell-mode-defaults ()
       (subword-mode +1)
       (eldoc-mode +1)
       (haskell-indentation-mode +1)
       (interactive-haskell-mode +1))

     (setq prelude-haskell-mode-hook 'prelude-haskell-mode-defaults)

     (add-hook 'haskell-mode-hook (lambda ()
                                    (run-hooks 'prelude-haskell-mode-hook)))))

(require 'ido)
;; (require 'ido-ubiquitous)
;; (require 'flx-ido)

(ido-mode +1)
;; (ido-ubiquitous-mode +1)

;;; smarter fuzzy matching for ido
;; (flx-ido-mode +1)
;; disable ido faces to see flx highlights

;;; smex, remember recently and most frequently used commands
;; (require 'smex)
;; (smex-initialize)
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)

(provide 'prelude-ido)

(add-to-list 'auto-mode-alist '("\\.js\\'"    . js2-mode))
(add-to-list 'auto-mode-alist '("\\.pac\\'"   . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(eval-after-load 'js2-mode
  '(progn
     (defun prelude-js-mode-defaults ()
       ;; electric-layout-mode doesn't play nice with smartparens
       (setq-local electric-layout-rules '((?\; . after)))
       (setq mode-name "JS2")
       (js2-imenu-extras-mode +1))

     (setq prelude-js-mode-hook 'prelude-js-mode-defaults)

     (add-hook 'js2-mode-hook (lambda () (run-hooks 'prelude-js-mode-hook)))))

;; (require 'smartparens-latex)
;; for case
(require 'cl)

(defcustom prelude-latex-fast-math-entry 'LaTeX-math-mode
  "Method used for fast math symbol entry in LaTeX."
  :link '(function-link :tag "AUCTeX Math Mode" LaTeX-math-mode)
  :link '(emacs-commentary-link :tag "CDLaTeX" "cdlatex.el")
  :group 'prelude
  :type '(choice (const :tag "None" nil)
                 (const :tag "AUCTeX Math Mode" LaTeX-math-mode)
                 (const :tag "CDLaTeX" cdlatex)))

(setq-default TeX-master nil)

;; sensible defaults for OS X, other OSes should be covered out-of-the-box
(when (eq system-type 'darwin)
  (setq TeX-view-program-selection
        '((output-dvi "DVI Viewer")
          (output-pdf "PDF Viewer")
          (output-html "HTML Viewer")))

  (setq TeX-view-program-list
        '(("DVI Viewer" "open %o")
          ("PDF Viewer" "open %o")
          ("HTML Viewer" "open %o"))))

(defun prelude-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t))

(defun prelude-font-lock-comment-annotations ()
  "Highlight a bunch of well known comment annotations.
This functions should be added to the hooks of major modes for programming."
  (font-lock-add-keywords
   nil '(("\\<\\(\\(FIX\\(ME\\)?\\|TODO\\|OPTIMIZE\\|HACK\\|REFACTOR\\):\\)"
          1 font-lock-warning-face t))))

;; show the name of the current function definition in the modeline
(require 'which-func)
(which-function-mode 1)

;; in Emacs 24 programming major modes generally derive from a common
;; mode named prog-mode; for others, we'll arrange for our mode
;; defaults function to run prelude-prog-mode-hook directly.  To
;; augment and/or counteract these defaults your own function
;; to prelude-prog-mode-hook, using:
;;
;;     (add-hook 'prelude-prog-mode-hook 'my-prog-mode-defaults t)
;;
;; (the final optional t sets the *append* argument)

;; smart curly braces
;; (sp-pair "{" nil :post-handlers
;;          '(((lambda (&rest _ignored)
;;               (crux-smart-open-line-above)) "RET")))

(defun prelude-prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (when prelude-guru
    (guru-mode +1))
  (smartparens-mode +1)
  (prelude-enable-whitespace)
  (prelude-local-comment-auto-fill)
  (prelude-font-lock-comment-annotations))

(setq prelude-prog-mode-hook 'prelude-prog-mode-defaults)

(add-hook 'prog-mode-hook (lambda ()
                            (run-hooks 'prelude-prog-mode-hook)))

;; enable on-the-fly syntax checking
(if (fboundp 'global-flycheck-mode)
    (global-flycheck-mode +1)
  (add-hook 'prog-mode-hook 'flycheck-mode))

(defun prelude-latex-mode-defaults ()
  "Default Prelude hook for `LaTeX-mode'."
  (turn-on-auto-fill)
  (abbrev-mode +1)
  (smartparens-mode +1)
  (case prelude-latex-fast-math-entry
    (LaTeX-math-mode (LaTeX-math-mode 1))
    (cdlatex (turn-on-cdlatex))))

;; recognize prezto files as zsh scripts
(defvar prelude-prezto-files '("zlogin" "zlogin" "zlogout" "zpreztorc" "zprofile" "zshenv" "zshrc"))

(mapc (lambda (file)
        (add-to-list 'auto-mode-alist `(,(format "\\%s\\'" file) . sh-mode)))
      prelude-prezto-files)

(add-hook 'sh-mode-hook
          (lambda ()
            (if (and buffer-file-name
                     (member (file-name-nondirectory buffer-file-name) prelude-prezto-files))
                (sh-set-shell "zsh"))))

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
