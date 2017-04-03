;;; keymap.el -- Emacs keymap

;;; Commentary:

;;; Code:

(autoload 'org-cycle "org" nil t)
(autoload 'hippie-expand "hippie-exp" nil t)
(autoload 'indent-according-to-mode "indent" nil t)

(defun smart-tab (&optional arg)
  "Smart tab.
ARG unused"
  (interactive "P")
  (cond
   ((looking-back "^[-+* \t]*" nil)
    (if (eq major-mode 'org-mode)
        (org-cycle arg)
      (indent-according-to-mode)))
   (t
    ;; Hippie also expands yasnippets, due to `yas-hippie-try-expand' in
    ;; `hippie-expand-try-functions-list'.
    (hippie-expand arg))))

(define-key key-translation-map (kbd "A-TAB") (kbd "C-TAB"))

;;; C-

(bind-key* "<C-return>" #'other-window)

(defun collapse-or-expand ()
  "Collapse or expand."
  (interactive)
  (if (> (length (window-list)) 1)
      (delete-other-windows)
    (bury-buffer)))

(bind-key "C-z" #'delete-other-windows)

;;; M-

(defadvice async-shell-command (before uniqify-running-shell-command
                                       activate)
  "Run shell command async."
  (let ((buf (get-buffer "*Async Shell Command*")))
    (if buf
        (let ((proc (get-buffer-process buf)))
          (if (and proc (eq 'run (process-status proc)))
              (with-current-buffer buf
                (rename-uniquely)))))))

(bind-key "M-!" #'async-shell-command)
(bind-key "M-'" #'insert-pair)
(bind-key "M-\"" #'insert-pair)
(bind-key "M-`" #'other-frame)

(bind-key "M-j" #'delete-indentation-forward)
(bind-key "M-J" #'delete-indentation)

(bind-key "M-W" #'mark-word)

(defun mark-line (&optional arg)
  "Mark line.
ARG unused"
  (interactive "p")
  (beginning-of-line)
  (let ((here (point)))
    (dotimes (i arg)
      (end-of-line))
    (set-mark (point))
    (goto-char here)))

(bind-key "M-L" #'mark-line)

(defun mark-sentence (&optional arg)
  "Mark sentence.
ARG unused"
  (interactive "P")
  (backward-sentence)
  (mark-end-of-sentence arg))

(bind-key "M-S" #'mark-sentence)
(bind-key "M-X" #'mark-sexp)
(bind-key "M-D" #'mark-defun)

(bind-key "M-g c" #'goto-char)
(bind-key "M-g l" #'goto-line)

(defun delete-indentation-forward ()
  "Delete indentation forward."
  (interactive)
  (delete-indentation t))

;;; M-C-

(bind-key "<C-M-backspace>" #'backward-kill-sexp)

;;; ctl-x-map

;;; C-x

(defvar edit-rectangle-origin)
(defvar edit-rectangle-saved-window-config)

(defun edit-rectangle (&optional start end)
  "Edit rectangle.
START unused
END unused"
  (interactive "r")
  (let ((strs (delete-extract-rectangle start end))
        (mode major-mode)
        (here (copy-marker (min (mark) (point)) t))
        (config (current-window-configuration)))
    (with-current-buffer (generate-new-buffer "*Rectangle*")
      (funcall mode)
      (set (make-local-variable 'edit-rectangle-origin) here)
      (set (make-local-variable 'edit-rectangle-saved-window-config) config)
      (local-set-key (kbd "C-c C-c") #'restore-rectangle)
      (mapc #'(lambda (x) (insert x ?\n)) strs)
      (goto-char (point-min))
      (pop-to-buffer (current-buffer)))))

(defun restore-rectangle ()
  (interactive)
  (let ((content (split-string (buffer-string) "\n"))
        (origin edit-rectangle-origin)
        (config edit-rectangle-saved-window-config))
    (with-current-buffer (marker-buffer origin)
      (goto-char origin)
      (insert-rectangle content))
    (kill-buffer (current-buffer))
    (set-window-configuration config)))

(bind-key "C-x D" #'edit-rectangle)
(bind-key "C-x d" #'delete-whitespace-rectangle)
(bind-key "C-x F" #'set-fill-column)
(bind-key "C-x t" #'toggle-truncate-lines)

(defun delete-current-buffer-file ()
  "Delete the current buffer and the file connected with it."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (kill-buffer buffer)
      (when (yes-or-no-p "Are you sure, want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

(bind-key "C-x v H" #'vc-region-history)
(bind-key "C-x K" #'delete-current-buffer-file)

;;; C-x C-

(defun duplicate-line ()
  "Duplicate the line containing point."
  (interactive)
  (save-excursion
    (let (line-text)
      (goto-char (line-beginning-position))
      (let ((beg (point)))
        (goto-char (line-end-position))
        (setq line-text (buffer-substring beg (point))))
      (if (eobp)
          (insert ?\n)
        (forward-line))
      (open-line 1)
      (insert line-text))))

(bind-key "C-x C-d" #'duplicate-line)
(bind-key "C-x C-e" #'pp-eval-last-sexp)
(bind-key "C-x C-n" #'next-line)

(defun find-alternate-file-with-sudo ()
  "Find alternate file with sudo."
  (interactive)
  (find-alternate-file (concat "/sudo::" (buffer-file-name))))

(bind-key "C-x C-v" #'find-alternate-file-with-sudo)

;;; C-x M-

(bind-key "C-x M-n" #'set-goal-column)

(defun refill-paragraph (arg)
  "Refill paragraph.
ARG unused"
  (interactive "*P")
  (let ((fun (if (memq major-mode '(c-mode c++-mode))
                 'c-fill-paragraph
               (or fill-paragraph-function
                   'fill-paragraph)))
        (width (if (numberp arg) arg))
        prefix beg end)
    (forward-paragraph 1)
    (setq end (copy-marker (- (point) 2)))
    (forward-line -1)
    (let ((b (point)))
      (skip-chars-forward "^A-Za-z0-9`'\"(")
      (setq prefix (buffer-substring-no-properties b (point))))
    (backward-paragraph 1)
    (if (eolp)
        (forward-char))
    (setq beg (point-marker))
    (delete-horizontal-space)
    (while (< (point) end)
      (delete-indentation 1)
      (end-of-line))
    (let ((fill-column (or width fill-column))
          (fill-prefix prefix))
      (if prefix
          (setq fill-column
                (- fill-column (* 2 (length prefix)))))
      (funcall fun nil)
      (goto-char beg)
      (insert prefix)
      (funcall fun nil))
    (goto-char (+ end 2))))

(bind-key "C-x M-q" #'refill-paragraph)

(defun endless/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'endless/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph]
                #'endless/fill-or-unfill)

;;; mode-specific-map

;;; C-c

(bind-key "C-c <tab>" #'ff-find-other-file)
(bind-key "C-c SPC" #'just-one-space)

(defmacro recursive-edit-preserving-window-config (body)
  "*Return a command that enters a recursive edit after executing BODY.
Upon exiting the recursive edit (with\\[exit-recursive-edit] (exit)
or \\[abort-recursive-edit] (abort)), restore window configuration
in current frame.
Inspired by Erik Naggum's `recursive-edit-with-single-window'."
  `(lambda ()
     "See the documentation for `recursive-edit-preserving-window-config'."
     (interactive)
     (save-window-excursion
       ,body
       (recursive-edit))))

(bind-key "C-c 0"
  (recursive-edit-preserving-window-config (delete-window)))
(bind-key "C-c 1"
  (recursive-edit-preserving-window-config
   (if (one-window-p 'ignore-minibuffer)
       (error "Current window is the only window in its frame")
     (delete-other-windows))))

(defun delete-current-line (&optional arg)
  "Delete current line.
ARG unused"
  (interactive "p")
  (let ((here (point)))
    (beginning-of-line)
    (kill-line arg)
    (goto-char here)))

(bind-key "C-c d" #'delete-current-line)
(bind-key "C-c g" #'goto-line)

(defun do-eval-buffer ()
  "Eval buffer."
  (interactive)
  (call-interactively 'eval-buffer)
  (message "Buffer has been evaluated"))

(defun do-eval-region ()
  "Eval region."
  (interactive)
  (call-interactively 'eval-region)
  (message "Region has been evaluated"))

(bind-keys :prefix-map my-lisp-devel-map
           :prefix "C-c e"
           ("E" . elint-current-buffer)
           ("b" . do-eval-buffer)
           ("c" . cancel-debug-on-entry)
           ("d" . debug-on-entry)
           ("e" . toggle-debug-on-error)
           ("f" . emacs-lisp-byte-compile-and-load)
           ("j" . emacs-lisp-mode)
           ("l" . find-library)
           ("r" . do-eval-region)
           ("s" . scratch)
           ("z" . byte-recompile-directory))

(bind-key "C-c f" #'flush-lines)
(bind-key "C-c k" #'keep-lines)

(defcustom user-initials nil
  "*Initials of this user."
  :set
  #'(lambda (symbol value)
      (if (fboundp 'font-lock-add-keywords)
          (mapc
           #'(lambda (mode)
               (font-lock-add-keywords
                mode (list (list (concat "\\<\\(" value " [^:\n]+\\):")
                                 1 font-lock-warning-face t))))
           '(c-mode c++-mode emacs-lisp-mode lisp-mode
                    python-mode perl-mode java-mode groovy-mode
                    haskell-mode literate-haskell-mode)))
      (set symbol value))
  :type 'string
  :group 'mail)

(defun insert-user-timestamp ()
  "Insert a quick timestamp using the value of `user-initials'."
  (interactive)
  (insert (format "%s (%s): " user-initials
                  (format-time-string "%Y-%m-%d" (current-time)))))

(bind-key "C-c n" #'insert-user-timestamp)
(bind-key "C-c o" #'customize-option)
(bind-key "C-c O" #'customize-group)
(bind-key "C-c F" #'customize-face)

(bind-key "C-c q" #'fill-region)
(bind-key "C-c r" #'replace-regexp)
(bind-key "C-c s" #'replace-string)
(bind-key "C-c u" #'rename-uniquely)

(bind-key "C-c v" #'ffap)

(defun view-clipboard ()
  "View clipboard."
  (interactive)
  (delete-other-windows)
  (switch-to-buffer "*Clipboard*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (clipboard-yank)
    (goto-char (point-min))))

(bind-key "C-c V" #'view-clipboard)
(bind-key "C-c z" #'clean-buffer-list)

(bind-key "C-c =" #'count-matches)
(bind-key "C-c ;" #'comment-or-uncomment-region)

;;; C-c C-

(defun delete-to-end-of-buffer ()
  "Delete to end of buffer."
  (interactive)
  (kill-region (point) (point-max)))

(bind-key "C-c C-z" #'delete-to-end-of-buffer)

(defun copy-current-buffer-name ()
  "Copy current buffer name."
  (interactive)
  (let ((name (buffer-file-name)))
    (kill-new name)
    (message name)))

(bind-key "C-c C-0" #'copy-current-buffer-name)

;;; C-c M-

(defun unfill-paragraph (arg)
  "Unfill paragraph.
ARG unused"
  (interactive "*p")
  (let (beg end)
    (forward-paragraph arg)
    (setq end (copy-marker (- (point) 2)))
    (backward-paragraph arg)
    (if (eolp)
        (forward-char))
    (setq beg (point-marker))
    (when (> (count-lines beg end) 1)
      (while (< (point) end)
        (goto-char (line-end-position))
        (let ((sent-end (memq (char-before) '(?. ?\; ?! ??))))
          (delete-indentation 1)
          (if sent-end
              (insert ? )))
        (end-of-line))
      (save-excursion
        (goto-char beg)
        (while (re-search-forward "[^.;!?:]\\([ \t][ \t]+\\)" end t)
          (replace-match " " nil nil nil 1))))))

(bind-key "C-c M-q" #'unfill-paragraph)

(defun unfill-region (beg end)
  "Unfill region.
BEG beginning of region
END end of region"
  (interactive "r")
  (setq end (copy-marker end))
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (unfill-paragraph 1)
      (forward-paragraph))))

;;; C-.

(bind-key "C-. m" #'kmacro-keymap)

(bind-key "C-. C-i" #'indent-rigidly)

;; Start a regular shell if you prefer that.
(global-set-key (kbd "C-x M-m") 'shell)

(fset 'yes-or-no-p 'y-or-n-p)

(bind-key "C-x p" 'pop-to-mark-command)
(setq set-mark-command-repeat-pop t)

(provide 'keymap)
;;; keymap.el ends here
