(defvar lisp-modes '(emacs-lisp-mode
                     inferior-emacs-lisp-mode
                     ielm-mode
                     lisp-mode
                     inferior-lisp-mode
                     lisp-interaction-mode
                     slime-repl-mode))

(defvar lisp-mode-hooks
  (mapcar (function
           (lambda (mode)
             (intern
              (concat (symbol-name mode) "-hook"))))
          lisp-modes))

(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

(use-package ace-window
  :commands ace-window
  :bind ("M-o" . ace-window))

(use-package ag
  :commands ag
  :if (executable-find "ag")
  :bind ("C-?" . ag-project))

(use-package aggressive-indent
  :commands aggressive-indent-mode
  :init
  (apply #'hook-into-modes 'aggressive-indent-mode lisp-mode-hooks))

(use-package align
  :bind (("M-["   . align-code)
         ("C-c [" . align-regexp))
  :commands align
  :preface
  (defun align-code (beg end &optional arg)
    (interactive "rP")
    (if (null arg)
        (align beg end)
      (let ((end-mark (copy-marker end)))
        (indent-region beg end-mark nil)
        (align beg end-mark)))))

(use-package anaconda-mode
  :commands (anaconda-mode anaconda-eldoc-mode)
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package async
  :disabled
  :commands dired-async-mode
  :init
  (dired-async-mode 1))

(use-package apropospriate-theme
  :demand
  :config
  (load-theme 'apropospriate-dark))

(use-package apu
  :bind ("C-h u" . apropos-char)
  :commands apropos-char)

(use-package auto-compile
  :demand
  :disabled
  :config
  (auto-compile-on-save-mode)
  (auto-compile-on-load-mode))

(use-package auto-highlight-symbol
  :disabled
  :commands auto-highlight-symbol-mode
  :init
  (add-hook 'prog-mode-hook auto-highlight-symbol-mode))

(use-package autopair
  :disabled
  :commands autopair-global-mode
  :init (autopair-global-mode))

(use-package autorevert
  :commands auto-revert-mode
  :init
  (add-hook 'find-file-hook #'(lambda () (auto-revert-mode 1))))

(use-package avy
  :commands avy)

(use-package backup-each-save
  :disabled
  :commands backup-each-save
  :preface
  (defun show-backups ()
    (interactive)
    (require 'find-dired)
    (let* ((file (make-backup-file-name (buffer-file-name)))
           (dir (file-name-directory file))
           (args (concat "-iname '" (file-name-nondirectory file)
                         ".~*~'"))
           (dired-buffers dired-buffers)
           (find-ls-option '("-print0 | xargs -0 ls -lta" . "-lta")))
      ;; Check that it's really a directory.
      (or (file-directory-p dir)
          (error "Backup directory does not exist: %s" dir))
      (with-current-buffer (get-buffer-create "*Backups*")
        (let ((find (get-buffer-process (current-buffer))))
          (when find
            (if (or (not (eq (process-status find) 'run))
                    (yes-or-no-p "A `find' process is running; kill it? "))
                (condition-case nil
                    (progn
                      (interrupt-process find)
                      (sit-for 1)
                      (delete-process find))
                  (error nil))
              (error "Cannot have two processes in `%s' at once"
                     (buffer-name)))))

        (widen)
        (kill-all-local-variables)
        (setq buffer-read-only nil)
        (erase-buffer)
        (setq default-directory dir
              args (concat
                    find-program " . "
                    (if (string= args "")
                        ""
                      (concat
                       (shell-quote-argument "(")
                       " " args " "
                       (shell-quote-argument ")")
                       " "))
                    (if (string-match "\\`\\(.*\\) {} \\(\\\\;\\|+\\)\\'"
                                      (car find-ls-option))
                        (format "%s %s %s"
                                (match-string 1 (car find-ls-option))
                                (shell-quote-argument "{}")
                                find-exec-terminator)
                      (car find-ls-option))))
        ;; Start the find process.
        (message "Looking for backup files...")
        (shell-command (concat args "&") (current-buffer))
        ;; The next statement will bomb in classic dired (no optional arg
        ;; allowed)
        (dired-mode dir (cdr find-ls-option))
        (let ((map (make-sparse-keymap)))
          (set-keymap-parent map (current-local-map))
          (define-key map "\C-c\C-k" 'kill-find)
          (use-local-map map))
        (make-local-variable 'dired-sort-inhibit)
        (setq dired-sort-inhibit t)
        (set (make-local-variable 'revert-buffer-function)
             `(lambda (ignore-auto noconfirm)
                (find-dired ,dir ,find-args)))
        ;; Set subdir-alist so that Tree Dired will work:
        (if (fboundp 'dired-simple-subdir-alist)
            ;; will work even with nested dired format (dired-nstd.el,v 1.15
            ;; and later)
            (dired-simple-subdir-alist)
          ;; else we have an ancient tree dired (or classic dired, where
          ;; this does no harm)
          (set (make-local-variable 'dired-subdir-alist)
               (list (cons default-directory (point-min-marker)))))
        (set (make-local-variable 'dired-subdir-switches)
             find-ls-subdir-switches)
        (setq buffer-read-only nil)
        ;; Subdir headlerline must come first because the first marker in
        ;; subdir-alist points there.
        (insert "  " dir ":\n")
        ;; Make second line a ``find'' line in analogy to the ``total'' or
        ;; ``wildcard'' line.
        (insert "  " args "\n")
        (setq buffer-read-only t)
        (let ((proc (get-buffer-process (current-buffer))))
          (set-process-filter proc (function find-dired-filter))
          (set-process-sentinel proc (function find-dired-sentinel))
          ;; Initialize the process marker; it is used by the filter.
          (move-marker (process-mark proc) 1 (current-buffer)))
        (setq mode-line-process '(":%s")))))

  (bind-key "C-x ~" #'show-backups)

  :init
  (defun my-make-backup-file-name (file)
    (make-backup-file-name-1 (file-truename file)))

  (add-hook 'after-save-hook 'backup-each-save)

  :config
  (defun backup-each-save-filter (filename)
    (not (string-match
          (concat "\\(^/tmp\\|\\.emacs\\.d/data\\(-alt\\)?/"
                  "\\|\\.newsrc\\(\\.eld\\)?\\|"
                  "\\(archive/sent/\\|recentf\\`\\)\\)")
          filename)))

  (setq backup-each-save-filter-function 'backup-each-save-filter)

  (defun my-dont-backup-files-p (filename)
    (unless (string-match filename "\\(archive/sent/\\|recentf\\`\\)")
      (normal-backup-enable-predicate filename)))

  (setq backup-enable-predicate 'my-dont-backup-files-p))

(use-package bbdb
  :disabled)

(use-package bookmark
  :disabled
  :config
  (use-package bookmark+))

(use-package buffer-move
  :commands (buf-move-up buf-move-down buf-move-left buf-move-right)
  :bind
  (("<M-S-up>" . buf-move-up)
   ("<M-S-down>" . buf-move-down)
   ("<M-S-left>" . buf-move-left)
   ("<M-S-right>" . buf-move-right)))

(use-package bug-reference-github
  :init
  (add-hook 'prog-mode-hook 'bug-reference-prog-mode)
  :config
  (bug-reference-github-set-url-format))

(use-package cc-mode
  :mode (("\\.h\\(h?\\|xx\\|pp\\)\\'" . c++-mode)
         ("\\.m\\'"                   . c-mode)
         ("\\.c\\'"                   . c-mode)
         ("\\.cpp\\'"                 . c++-mode)
         ("\\.c++\\'"                 . c++-mode)
         ("\\.mm\\'"                  . c++-mode))
  :preface
  (defun my-paste-as-check ()
    (interactive)
    (save-excursion
      (insert "/*\n")
      (let ((beg (point)) end)
        (yank)
        (setq end (point-marker))
        (goto-char beg)
        (while (< (point) end)
          (forward-char 2)
          (insert "CHECK: ")
          (forward-line 1)))
      (insert "*/\n")))

  (defvar printf-index 0)

  (defun insert-counting-printf (arg)
    (interactive "P")
    (if arg
        (setq printf-index 0))
    (if t
        (insert (format "std::cerr << \"step %d..\" << std::endl;\n"
                        (setq printf-index (1+ printf-index))))
      (insert (format "printf(\"step %d..\\n\");\n"
                      (setq printf-index (1+ printf-index)))))
    (forward-line -1)
    (indent-according-to-mode)
    (forward-line))

  (defun my-c-mode-common-hook ()
    ;; (ggtags-mode 1)
    (hs-minor-mode 1)
    (hide-ifdef-mode 1)
    ;; (whitespace-mode 1)
    (which-function-mode 1)
    (company-mode 1)
    (bug-reference-prog-mode 1)

    (bind-key "<return>" #'newline-and-indent c-mode-base-map)

    (unbind-key "M-j" c-mode-base-map)
    (bind-key "C-c C-i" #'c-includes-current-file c-mode-base-map)
    (bind-key "C-c C-y" #'my-paste-as-check c-mode-base-map)

    (set (make-local-variable 'parens-require-spaces) nil)
    (setq indicate-empty-lines t)
    (setq fill-column 72)

    (bind-key "M-q" #'c-fill-paragraph c-mode-base-map)

    (let ((bufname (buffer-file-name)))
      (when bufname
        (cond
         ((string-match "/ledger/" bufname)
          (c-set-style "ledger"))
         ((string-match "/edg/" bufname)
          (c-set-style "edg"))
         (t
          (c-set-style "clang")))))

    (font-lock-add-keywords 'c++-mode '(("\\<\\(assert\\|DEBUG\\)("
                                         1 font-lock-warning-face t))))

  :config

  (use-package c-eldoc
    :commands c-turn-on-eldoc-mode
    :init
    (add-hook 'c-mode-common-hook 'c-turn-on-eldoc-mode)
    )

  (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

  (setq c-syntactic-indentation nil)

  (bind-key "#" #'self-insert-command c-mode-base-map)
  (bind-key "{" #'self-insert-command c-mode-base-map)
  (bind-key "}" #'self-insert-command c-mode-base-map)
  (bind-key "/" #'self-insert-command c-mode-base-map)
  (bind-key "*" #'self-insert-command c-mode-base-map)
  (bind-key ";" #'self-insert-command c-mode-base-map)
  (bind-key "," #'self-insert-command c-mode-base-map)
  (bind-key ":" #'self-insert-command c-mode-base-map)
  (bind-key "(" #'self-insert-command c-mode-base-map)
  (bind-key ")" #'self-insert-command c-mode-base-map)
  (bind-key "<" #'self-insert-command c++-mode-map)
  (bind-key ">" #'self-insert-command c++-mode-map)

  (add-to-list 'c-style-alist
               '("edg"
                 (indent-tabs-mode . nil)
                 (c-basic-offset . 2)
                 (c-comment-only-line-offset . (0 . 0))
                 (c-hanging-braces-alist
                  . ((substatement-open before after)
                     (arglist-cont-nonempty)))
                 (c-offsets-alist
                  . ((statement-block-intro . +)
                     (knr-argdecl-intro . 5)
                     (substatement-open . 0)
                     (substatement-label . 0)
                     (label . 0)
                     (case-label . +)
                     (statement-case-open . 0)
                     (statement-cont . +)
                     (arglist-intro . +)
                     (arglist-close . +)
                     (inline-open . 0)
                     (brace-list-open . 0)
                     (topmost-intro-cont
                      . (first c-lineup-topmost-intro-cont
                               c-lineup-gnu-DEFUN-intro-cont))))
                 (c-special-indent-hook . c-gnu-impose-minimum)
                 (c-block-comment-prefix . "")))

  (add-to-list 'c-style-alist
               '("ledger"
                 (indent-tabs-mode . nil)
                 (c-basic-offset . 2)
                 (c-comment-only-line-offset . (0 . 0))
                 (c-hanging-braces-alist
                  . ((substatement-open before after)
                     (arglist-cont-nonempty)))
                 (c-offsets-alist
                  . ((statement-block-intro . +)
                     (knr-argdecl-intro . 5)
                     (substatement-open . 0)
                     (substatement-label . 0)
                     (label . 0)
                     (case-label . 0)
                     (statement-case-open . 0)
                     (statement-cont . +)
                     (arglist-intro . +)
                     (arglist-close . +)
                     (inline-open . 0)
                     (brace-list-open . 0)
                     (topmost-intro-cont
                      . (first c-lineup-topmost-intro-cont
                               c-lineup-gnu-DEFUN-intro-cont))))
                 (c-special-indent-hook . c-gnu-impose-minimum)
                 (c-block-comment-prefix . "")))

  (add-to-list 'c-style-alist
               '("clang"
                 (indent-tabs-mode . nil)
                 (c-basic-offset . 2)
                 (c-comment-only-line-offset . (0 . 0))
                 (c-hanging-braces-alist
                  . ((substatement-open before after)
                     (arglist-cont-nonempty)))
                 (c-offsets-alist
                  . ((statement-block-intro . +)
                     (knr-argdecl-intro . 5)
                     (substatement-open . 0)
                     (substatement-label . 0)
                     (label . 0)
                     (case-label . 0)
                     (statement-case-open . 0)
                     (statement-cont . +)
                     (arglist-intro . +)
                     (arglist-close . +)
                     (inline-open . 0)
                     (brace-list-open . 0)
                     (topmost-intro-cont
                      . (first c-lineup-topmost-intro-cont
                               c-lineup-gnu-DEFUN-intro-cont))))
                 (c-special-indent-hook . c-gnu-impose-minimum)
                 (c-block-comment-prefix . ""))))

(use-package cider
  :disabled)

(use-package cmake-mode
  :mode (("CMakeLists.txt" . cmake-mode)
         ("\\.cmake\\'"    . cmake-mode)))

(use-package coffee-mode
  :mode (("\\.coffee\\'"   . coffee-mode)))

(use-package company
  :bind ("<C-tab>" . company-complete)
  :diminish company-mode
  :commands company-mode
  ;; :after helm
  :config (global-company-mode 1)
  (add-hook 'after-init-hook 'global-company-mode)


  ;; ;; Use Helm to complete suggestions
  ;; (define-key company-mode-map (kbd "C-:") 'helm-company)
  ;; (define-key company-active-map (kbd "C-:") 'helm-company)
  ;; (define-key company-active-map (kbd "C-n") 'company-select-next)
  ;; (define-key company-active-map (kbd "C-p") 'company-select-previous)
  )

(use-package company-anaconda
  :disabled
  :after company
  :commands company-anaconda
  :init
  (add-to-list 'company-backends 'company-anaconda))

(use-package company-math
  :commands company-math-symbols-unicode
  :init
  (add-hook 'company-backends 'company-math-symbols-unicode))

(use-package company-statistics
  :commands company-statistics-mode
  :init
  (add-hook 'after-init-hook 'company-statistics-mode))

(use-package company-tern
  :disabled
  :commands company-tern
  :init (add-to-list 'company-backends 'company-tern))

(use-package company-quickhelp
  :disabled
  :commands company-quickhelp-mode
  :init (company-quickhelp-mode 1))

(use-package company-ycmd
  :after ycmd
  :commands company-ycmd-setup
  :init
  (add-hook 'after-init-hook #'company-ycmd-setup))

(use-package compile
  :bind (("C-c c" . compile)
         ("M-O"   . show-compilation))
  :preface
  (defun show-compilation ()
    (interactive)
    (let ((compile-buf
           (catch 'found
             (dolist (buf (buffer-list))
               (if (string-match "\\*compilation\\*" (buffer-name buf))
                   (throw 'found buf))))))
      (if compile-buf
          (switch-to-buffer-other-window compile-buf)
        (call-interactively 'compile))))

  (defun compilation-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
         (point-marker)))

  :config
  (add-hook 'compilation-filter-hook #'compilation-ansi-color-process-output))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("C-x C-f" . counsel-find-file)
         ("<f1> l" . counsel-find-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-c g" . counsel-git)
         ("C-c j" . counsel-git-grep)
         ("C-c k" . counsel-ag)
         ("C-x l" . counsel-locate)))

(use-package counsel-projectile
  :commands counsel-projectile-on
  :init (counsel-projectile-on))

(use-package crontab-mode
  :mode "\\.?cron\\(tab\\)?\\'")

(use-package css-mode
  :commands css-mode
  :config
  (use-package rainbow-mode
    :commands rainbow-mode
    :init
    (dolist (hook '(css-mode-hook html-mode-hook sass-mode-hook))
      (add-hook hook 'rainbow-mode)))
  (use-package css-eldoc))

(use-package cursor-chg
  :commands change-cursor-mode
  :config
  (change-cursor-mode 1)
  (toggle-cursor-type-when-idle 1))

(use-package csv-mode
  :mode "\\.csv\\'")

(use-package diffview
  :commands (diffview-current diffview-region diffview-message))

(use-package diminish)

(use-package dired
  :ensure nil
  :bind ("C-c J" . dired-double-jump)
  :preface
  (defvar mark-files-cache (make-hash-table :test #'equal))

  (defun mark-similar-versions (name)
    (let ((pat name))
      (if (string-match "^\\(.+?\\)-[0-9._-]+$" pat)
          (setq pat (match-string 1 pat)))
      (or (gethash pat mark-files-cache)
          (ignore (puthash pat t mark-files-cache)))))

  (defun dired-mark-similar-version ()
    (interactive)
    (setq mark-files-cache (make-hash-table :test #'equal))
    (dired-mark-sexp '(mark-similar-versions name)))

  (defun dired-double-jump (first-dir second-dir)
    (interactive
     (list (read-directory-name "First directory: "
                                (expand-file-name "~")
                                nil nil "dl/")
           (read-directory-name "Second directory: "
                                (expand-file-name "~")
                                nil nil "Archives/")))
    (dired first-dir)
    (dired-other-window second-dir))

  (defun my-dired-switch-window ()
    (interactive)
    (if (eq major-mode 'sr-mode)
        (call-interactively #'sr-change-window)
      (call-interactively #'other-window)))

  :config
  (bind-key "l" #'dired-up-directory dired-mode-map)

  (bind-key "<tab>" #'my-dired-switch-window dired-mode-map)

  (bind-key "M-!" #'async-shell-command dired-mode-map)
  (unbind-key "M-G" dired-mode-map)

  (use-package dired+
    :disabled
    :config
    (unbind-key "M-s f" dired-mode-map))

  (use-package dired-details
    ;; (shell-command "rm -f site-lisp/dired-details.el*")
    :disabled)

  (use-package dired-ranger
    :disabled
    :bind (:map dired-mode-map
                ("W" . dired-ranger-copy)
                ("X" . dired-ranger-move)
                ("Y" . dired-ranger-paste)))

  (use-package dired-sort-map
    ;; (shell-command "rm -f site-lisp/dired-sort-map.el*")
    :disabled)

  (use-package dired-toggle
    :disabled
    :load-path "site-lisp/dired-toggle"
    :bind ("C-. d" . dired-toggle)
    :preface
    (defun my-dired-toggle-mode-hook ()
      (interactive)
      (visual-line-mode 1)
      (setq-local visual-line-fringe-indicators '(nil right-curly-arrow))
      (setq-local word-wrap nil))
    :config
    (add-hook 'dired-toggle-mode-hook #'my-dired-toggle-mode-hook))

  (defadvice dired-omit-startup (after diminish-dired-omit activate)
    "Make sure to remove \"Omit\" from the modeline."
    (diminish 'dired-omit-mode) dired-mode-map)

  (defadvice dired-next-line (around dired-next-line+ activate)
    "Replace current buffer if file is a directory."
    ad-do-it
    (while (and  (not  (eobp)) (not ad-return-value))
      (forward-line)
      (setq ad-return-value(dired-move-to-filename)))
    (when (eobp)
      (forward-line -1)
      (setq ad-return-value(dired-move-to-filename))))

  (defadvice dired-previous-line (around dired-previous-line+ activate)
    "Replace current buffer if file is a directory."
    ad-do-it
    (while (and  (not  (bobp)) (not ad-return-value))
      (forward-line -1)
      (setq ad-return-value(dired-move-to-filename)))
    (when (bobp)
      (call-interactively 'dired-next-line)))

  (defvar dired-omit-regexp-orig (symbol-function 'dired-omit-regexp))

  ;; Omit files that Git would ignore
  (defun dired-omit-regexp ()
    (let ((file (expand-file-name ".git"))
          parent-dir)
      (while (and (not (file-exists-p file))
                  (progn
                    (setq parent-dir
                          (file-name-directory
                           (directory-file-name
                            (file-name-directory file))))
                    ;; Give up if we are already at the root dir.
                    (not (string= (file-name-directory file)
                                  parent-dir))))
        ;; Move up to the parent dir and try again.
        (setq file (expand-file-name ".git" parent-dir)))
      ;; If we found a change log in a parent, use that.
      (if (file-exists-p file)
          (let ((regexp (funcall dired-omit-regexp-orig))
                (omitted-files
                 (shell-command-to-string "git clean -d -x -n")))
            (if (= 0 (length omitted-files))
                regexp
              (concat
               regexp
               (if (> (length regexp) 0)
                   "\\|" "")
               "\\("
               (mapconcat
                #'(lambda (str)
                    (concat
                     "^"
                     (regexp-quote
                      (substring str 13
                                 (if (= ?/ (aref str (1- (length str))))
                                     (1- (length str))
                                   nil)))
                     "$"))
                (split-string omitted-files "\n" t)
                "\\|")
               "\\)")))
        (funcall dired-omit-regexp-orig)))))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config

  (dumb-jump-mode))

(use-package easy-kill
  :disabled
  :commands easy-kil
  :init
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package edebug
  :preface
  (defvar modi/fns-in-edebug nil
    "List of functions for which `edebug' is instrumented.")

  (defconst modi/fns-regexp
    (concat "(\\s-*"
            "\\(defun\\|defmacro\\)\\s-+"
            "\\(?1:\\(\\w\\|\\s_\\)+\\)\\_>") ; word or symbol char
    "Regexp to find defun or defmacro definition.")

  (defun modi/toggle-edebug-defun ()
    (interactive)
    (let (fn)
      (save-excursion
        (search-backward-regexp modi/fns-regexp)
        (setq fn (match-string 1))
        (mark-sexp)
        (narrow-to-region (point) (mark))
        (if (member fn modi/fns-in-edebug)
            ;; If the function is already being edebugged, uninstrument it
            (progn
              (setq modi/fns-in-edebug (delete fn modi/fns-in-edebug))
              (eval-region (point) (mark))
              (setq-default eval-expression-print-length 12)
              (setq-default eval-expression-print-level  4)
              (message "Edebug disabled: %s" fn))
          ;; If the function is not being edebugged, instrument it
          (progn
            (add-to-list 'modi/fns-in-edebug fn)
            (setq-default eval-expression-print-length nil)
            (setq-default eval-expression-print-level  nil)
            (edebug-defun)
            (message "Edebug: %s" fn)))
        (widen)))))

(use-package ediff
  :disabled
  :init
  (defvar ctl-period-equals-map)
  (define-prefix-command 'ctl-period-equals-map)
  (bind-key "C-. =" #'ctl-period-equals-map)

  :bind (("C-. = b" . ediff-buffers)
         ("C-. = B" . ediff-buffers3)
         ("C-. = c" . compare-windows)
         ("C-. = =" . ediff-files)
         ("C-. = f" . ediff-files)
         ("C-. = F" . ediff-files3)
         ("C-. = r" . ediff-revision)
         ("C-. = p" . ediff-patch-file)
         ("C-. = P" . ediff-patch-buffer)
         ("C-. = l" . ediff-regions-linewise)
         ("C-. = w" . ediff-regions-wordwise))

  :config
  (use-package ediff-keep))

(use-package editorconfig
  :demand
  :if (executable-find "editorconfig")
  :mode ("\\.editorconfig\\'" . conf-unix-mode)
  :config
  (editorconfig-mode 1))

(use-package emacs-lisp-mode
  :ensure nil
  :init
  (progn
    (use-package eldoc
      :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))
    (use-package macrostep
      :bind ("C-c e" . macrostep-expand))
    (use-package ert
      :config (add-to-list 'emacs-lisp-mode-hook 'ert--activate-font-lock-keywords)))
  :config
  (progn
    (setq tab-always-indent 'complete)
    (add-to-list 'completion-styles 'initials t))
  :bind (("M-." . find-function-at-point)
         ("M-&" . complete-symbol))
  :interpreter (("emacs" . emacs-lisp-mode)))

(use-package erc
  :bind ("C-x r c" . irc)
  :defines (erc-timestamp-only-if-changed-flag
            erc-timestamp-format
            erc-fill-prefix
            erc-fill-column
            erc-insert-timestamp-function
            erc-modified-channels-alist)
  :preface
  (defun irc ()
    (interactive)
    (require 'erc)
    (erc-tls :server "irc.freenode.net"
             :port 6697
             :nick "matthewbauer"))

  :config
  (erc-track-minor-mode 1)
  (erc-track-mode 1)
  (erc-services-mode 1)
  (add-hook 'erc-connect-pre-hook (lambda (x) (erc-update-modules))))

(use-package elpy
  :disabled)

(use-package esh-help
  :commands esh-help-eldoc-command
  :init
  (add-hook 'eshell-mode-hook 'eldoc-mode)
  (add-hook 'eshell-mode-hook
            (lambda ()
              (make-local-variable 'eldoc-documentation-function)
              (setq eldoc-documentation-function
                    'esh-help-eldoc-command))))

(use-package eshell-autojump
  :disabled
  :commands eshell/j)

(use-package eshell-fringe-status
  :commands eshell-fringe-status-mode
  :init
  (add-hook 'eshell-mode-hook 'eshell-fringe-status-mode))

(use-package eshell
  :bind ("C-x e" . eshell)
  :commands (eshell eshell-command)
  :preface
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

  :config

  (require 'esh-opt)

  ;; quick commands
  ;; (defalias 'eshell/e 'find-file-other-window)
  ;; (defalias 'eshell/d 'dired)
  (setenv "PAGER" "cat")
  (setenv "EDITOR" "emacsclient -nq")

  ;; support `em-smart'
  (require 'em-smart)
  (add-hook 'eshell-mode-hook 'eshell-smart-initialize))

(use-package eshell-prompt-extras
  :commands epe-theme-lambda
  :init
  (setq eshell-prompt-function 'epe-theme-lambda))

(use-package eshell-z
  :disabled
  :commands eshell/z)

(use-package esup
  :commands esup)

(use-package etags
  :bind ("M-T" . tags-search))

(use-package eyebrowse
  :disabled
  :commands eyebrowse-mode
  :init (eyebrowse-mode t))

(use-package fancy-narrow
  :commands (fancy-narrow-to-region fancy-widen))

(use-package fasd
  :config
  (global-fasd-mode 1))

(use-package flatland-theme
  :demand
  :disabled
  :config (load-theme 'flatland))

(use-package flycheck
  :commands global-flycheck-mode
  :init (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package flycheck-ycmd
  :commands flycheck-ycmd-setup
  :init (add-hook 'after-init-hook #'flycheck-ycmd-setup))

(use-package flycheck-pos-tip
  :disabled
  :commands flycheck-pos-tip-mode
  :after flycheck
  :init (flycheck-pos-tip-mode))

(use-package flyspell
  :commands flyspell-mode ;;(spell-checking/change-dictionary)
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
  ;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)
  )

(use-package gist)

(use-package gitattributes-mode
  :mode "\\.gitattributes\\'")

(use-package github-clone
  :commands github-clone)

(use-package gitconfig-mode
  :mode "\\.gitconfig\\'")

(use-package gitignore-mode)

(use-package git-timemachine
  :commands git-timemachine)

(use-package git-messenger
  :disabled)

(use-package gnus
  :commands gnus
  :bind (("C-M-g" . gnus) ("C-x n u" . gnus))
  :init
  (add-hook 'kill-emacs-hook (lambda ()
                               (when (boundp 'gnus-group-exit)
                                 (gnus-group-exit))))
  (add-hook 'gnus-group-mode-hook 'gnus-topic-mode))

(use-package go-mode
  :mode "\\.go\\'")

(use-package go-eldoc
  :commands go-eldoc-setup
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package golden-ratio
  :commands golden-ratio-mode)

(use-package grep
  :bind (("M-s d" . find-grep-dired)
         ("M-s F" . find-grep)
         ("M-s G" . grep))
  :config
  ;; (add-hook 'grep-mode-hook #'(lambda () (use-package grep-ed)))

  (grep-apply-setting 'grep-command "egrep -nH -e ")
  (grep-apply-setting
   'grep-find-command
   '("find . -type f -print0 | xargs -P4 -0 egrep -nH " . 49))
  )

(use-package gud
  :commands gud-gdb
  :bind ("C-. g" . show-debugger)
  :init
  (defun show-debugger ()
    (interactive)
    (let ((gud-buf
           (catch 'found
             (dolist (buf (buffer-list))
               (if (string-match "\\*gud-" (buffer-name buf))
                   (throw 'found buf))))))
      (if gud-buf
          (switch-to-buffer-other-window gud-buf)
        (call-interactively 'gud-gdb))))
  :config
  (bind-key "<f9>" #'gud-cont)
  (bind-key "<f10>" #'gud-next)
  (bind-key "<f11>" #'gud-step)
  (bind-key "S-<f11>" #'gud-finish))

(use-package guru-mode
  :disabled
  :commands guru-global-mode
  :init (guru-global-mode))

(use-package haskell-mode
  :mode "\\.hs\\'")

(use-package haml-mode
  :mode "\\.haml\\'")

(use-package helm
  :commands helm-mode
  :disabled
  :diminish helm-mode
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
    :demand
    :config (helm-descbinds-mode))
  (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)
  (define-key isearch-mode-map (kbd "C-o") 'helm-occur-from-isearch)
  ;; shell history.
  (define-key shell-mode-map (kbd "C-c C-l") 'helm-comint-input-ring)
  (substitute-key-definition 'find-tag 'helm-etags-select global-map)
  (setq projectile-completion-system 'helm)
  (helm-mode 1))

(use-package helm-ag
  :bind ("C-c a" . helm-ag)
  :commands helm-ag)

(use-package helm-company
  :commands helm-company)

(use-package helm-flyspell
  :disabled)

(use-package helm-gitignore
  :commands helm-gitignore)

(use-package helm-make
  :commands (helm-make helm-make-projectile))

(use-package helm-swoop
  :bind (("M-s o" . helm-swoop)
         ("M-s /" . helm-multi-swoop))
  :config
  (use-package helm-match-plugin
    :config
    (helm-match-plugin-mode 1)))

(use-package hideshow
  :ensure nil
  :commands (hs-minor-mode)
  :init
  (add-hook 'prog-mode-hook 'hs-minor-mode))

(use-package hungry-delete
  :disabled
  :commands global-hungry-delete-mode
  :init
  (global-hungry-delete-mode))

(use-package hydra
  :disabled
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out")))

(use-package ibuffer
  :bind ("C-x b" . ibuffer))

(use-package idris
  :disabled
  :mode ("\\.idr\\'" . idris-mode))

(use-package imenu-list
  :commands imenu-list)

(use-package indium
  :commands (indium-mode indium-interaction-mode indium-scratch))

(use-package intero
  :commands intero-mode
  :init (add-hook 'haskell-mode-hook 'intero-mode))

(use-package ispell)

(use-package ivy
  :bind (("C-c C-r" . ivy-resume)
         ("<f6>" . ivy-resume)
         ("C-x C-b" . ivy-switch-buffer))
  :diminish
  :commands ivy-mode
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (ivy-mode 1))

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (flycheck-add-mode 'javascript-eslint 'js2-mode)
  (flycheck-mode 1)

  (bind-key "M-n" #'flycheck-next-error js2-mode-map)
  (bind-key "M-p" #'flycheck-previous-error js2-mode-map))

(use-package js2-refactor
  :commands js2-refactor-mode
  :init (add-hook 'js2-mode-hook #'js2-refactor-mode))

(use-package json-mode
  :mode "\\.json\\'"
  :config
  (use-package json-reformat)
  (use-package json-snatcher))

(use-package json-snatcher
  :commands js-mode-bindings
  :preface
  (defun js-mode-bindings ()
    "Sets a hotkey for using the json-snatcher plugin"
    (when (string-match  "\\.json$" (buffer-name))
      (local-set-key (kbd "C-c C-g") 'jsons-print-path)))
  :init
  (add-hook 'js2-mode-hook 'js-mode-binding)
  (add-hook 'js-mode-hook 'js-mode-binding))

(use-package launchctl
  :commands launchctl)

(use-package latex-unicode-math-mode
  :commands (latex-unicode-math-mode latyex-unicode-convert-region latex-unicode-convert-buffer)
  :init
  (add-hook 'LaTeX-mode-hook 'latex-unicode-math-mode))

(use-package less-css-mode
  :mode "\\.json\\'"
  :commands less-css-mode
  ;; :config
  ;; (use-package js2-mode)
  ;; (use-package skewer-less)
  )

(use-package lisp-mode
  :ensure nil
  :preface
  (defface esk-paren-face
    '((((class color) (background dark))
       (:foreground "grey50"))
      (((class color) (background light))
       (:foreground "grey55")))
    "Face used to dim parentheses."
    :group 'starter-kit-faces)

  (defvar slime-mode nil)
  (defvar lisp-mode-initialized nil)

  (defun my-lisp-mode-hook ()
    (unless lisp-mode-initialized
      (setq lisp-mode-initialized t)

      ;; (use-package redshank
      ;;   :demand)

      (use-package elisp-slime-nav)

      (use-package edebug
        :demand)

      (use-package eldoc
        :commands eldoc-mode
        :demand
        :config
        (use-package eldoc-extension
          :disabled
          :init
          (add-hook 'emacs-lisp-mode-hook
                    #'(lambda () (require 'eldoc-extension)) t))
        ;; (eldoc-add-command 'paredit-backward-delete
        ;;                    'paredit-close-round)
        )

      (use-package cldoc
        :demand
        :ensure nil
        :commands (cldoc-mode turn-on-cldoc-mode))

      (use-package ert)

      (use-package elint
        :commands 'elint-initialize
        :preface
        (defun elint-current-buffer ()
          (interactive)
          (elint-initialize)
          (elint-current-buffer))

        :config
        (add-to-list 'elint-standard-variables 'current-prefix-arg)
        (add-to-list 'elint-standard-variables 'command-line-args-left)
        (add-to-list 'elint-standard-variables 'buffer-file-coding-system)
        (add-to-list 'elint-standard-variables 'emacs-major-version)
        (add-to-list 'elint-standard-variables 'window-system))

      ;; (use-package highlight-cl
      ;;   :init
      ;;   (apply #'hook-into-modes 'highlight-cl-add-font-lock-keywords lisp-mode-hooks))

      (defun my-elisp-indent-or-complete (&optional arg)
        (interactive "p")
        (call-interactively 'lisp-indent-line)
        (unless (or (looking-back "^\\s-*")
                    (bolp)
                    (not (looking-back "[-A-Za-z0-9_*+/=<>!?]+")))
          (call-interactively 'lisp-complete-symbol)))

      (defun my-lisp-indent-or-complete (&optional arg)
        (interactive "p")
        (if (or (looking-back "^\\s-*") (bolp))
            (call-interactively 'lisp-indent-line)
          (call-interactively 'slime-indent-and-complete-symbol)))

      (defun my-byte-recompile-file ()
        (save-excursion
          (byte-recompile-file buffer-file-name)))

      (use-package info-lookmore
        :ensure nil
        :demand
        :config
        (info-lookmore-elisp-cl)
        (info-lookmore-elisp-userlast)
        (info-lookmore-elisp-gnus)
        (info-lookmore-apropos-elisp))

      (use-package testcover
        :commands testcover-this-defun)

      (mapc (lambda (mode)
              (info-lookup-add-help
               :mode mode
               :regexp "[^][()'\" \t\n]+"
               :ignore-case t
               :doc-spec '(("(ansicl)Symbol Index" nil nil nil))))
            lisp-modes)
      )

    (auto-fill-mode 1)
    ;; (paredit-mode 1)
    ;; (redshank-mode 1)
    ;; (elisp-slime-nav-mode 1)

    ;; (local-set-key (kbd "<return>") 'paredit-newline)
    (bind-key "<tab>" #'my-elisp-indent-or-complete emacs-lisp-mode-map)

    (add-hook 'after-save-hook 'check-parens nil t)

    (unless (memq major-mode
                  '(emacs-lisp-mode inferior-emacs-lisp-mode ielm-mode))
      (turn-on-cldoc-mode)
      (bind-key "M-q" #'slime-reindent-defun lisp-mode-map)
      (bind-key "M-l" #'slime-selector lisp-mode-map))
    )

  ;; Change lambda to an actual lambda symbol
  :init
  (mapc
   (lambda (major-mode)
     (font-lock-add-keywords
      major-mode
      '(("(\\(lambda\\)\\>"
         (0 (ignore
             (compose-region (match-beginning 1)
                             (match-end 1) ?λ))))
        ("(\\|)" . 'esk-paren-face)
        ("(\\(ert-deftest\\)\\>[         '(]*\\(setf[    ]+\\sw+\\|\\sw+\\)?"
         (1 font-lock-keyword-face)
         (2 font-lock-function-name-face
            nil t)))))
   lisp-modes)

  (apply #'hook-into-modes 'my-lisp-mode-hook lisp-mode-hooks))

(use-package lua-mode
  :mode "\\.lua\\'")

(use-package lsp-mode
  :disabled
  :commands global-lsp-mode
  :init
  (global-lsp-mode t)
  (with-eval-after-load 'lsp-mode
    (require 'lsp-flycheck))
  :config
  (lsp-define-client 'nix-mode "nix" 'stdio #'(lambda () default-directory)
                     :command
                     '("stack" "exec" "nix-language-server")
                     :name "Nix Language Server"
                     :ignore-regexps '())
  )

(use-package magit
  :commands (magit-clone)
  :if (executable-find "git")
  :bind (("C-x g" . magit-status)
         ("C-x G" . magit-dispatch-popup))
  )

(use-package magit-gh-pulls
  :if (executable-find "git")
  :commands turn-on-magit-gh-pulls
  :init
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

(use-package magithub
  :if (executable-find "git")
  :after magit
  :config (magithub-feature-autoinject t))

(use-package material-theme
  :disabled
  :demand
  :config
  (load-theme 'material t))

(use-package markdown-mode
  :mode "\\.\\(md\\|markdown\\)\\'"
  :commands markdown-mode
  :config
  (use-package markdown-preview-mode
    :config
    (setq markdown-preview-stylesheets
          (list "http://ftp.newartisans.com/pub/github.css")))
  ;; (use-package pandoc-mode
  ;;   :commands turn-on-pandoc
  ;;   :init
  ;;   (add-hook 'markdown-mode-hook 'turn-on-pandoc)
  )

(use-package mmm-mode
  :disabled
  :commands mmm-mode
  :init
  (setq mmm-global-mode 'maybe)
  (require 'mmm-auto))

(use-package multi-term
  :bind (("C-. t" . multi-term-next)
         ("C-. T" . multi-term))
  :init
  (defun screen ()
    (interactive)
    (let (term-buffer)
      ;; Set buffer.
      (setq term-buffer
            (let ((multi-term-program (executable-find "screen"))
                  (multi-term-program-switches "-DR"))
              (multi-term-get-buffer)))
      (set-buffer term-buffer)
      ;; Internal handle for `multi-term' buffer.
      (multi-term-internal)
      ;; Switch buffer
      (switch-to-buffer term-buffer)))

  :config
  (defalias 'my-term-send-raw-at-prompt 'term-send-raw)

  (defun my-term-end-of-buffer ()
    (interactive)
    (call-interactively #'end-of-buffer)
    (if (and (eobp) (bolp))
        (delete-char -1)))

  (require 'term)

  (defadvice term-process-pager (after term-process-rebind-keys activate)
    (define-key term-pager-break-map  "\177" 'term-pager-back-page)))

(use-package multishell
  :disabled)

(use-package minimap
  :commands minimap-mode)

(use-package monokai-theme
  :disabled
  :demand
  :config
  (load-theme 'monokai t))

(use-package multiple-cursors
  :commands (mc/mark-next-like-this mc/mark-previous-like-this)
  :bind
  (("<C-S-down>" . mc/mark-next-like-this)
   ("<C-S-up>" . mc/mark-previous-like-this)))

(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

(use-package nix-mode
  :load-path "~/Projects/nix-mode"
  :mode "\\.nix\\'")

(use-package org
  ;; :mode "\\.\\(org\\)\\'"
  :init
  (add-hook 'org-mode-hook 'auto-fill-mode))

(use-package org-bullets
  :commands org-bullets-mode
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package origami
  :commands origami-mode
  :init
  (add-hook 'prog-mode-hook 'origami-mode))

(use-package page-break-lines
  :init
  (add-hook 'doc-mode-hook 'page-break-lines-mode))

(use-package paredit
  :commands paredit-mode
  :disabled
  :init
  (apply #'hook-into-modes 'paredit-mode lisp-mode-hooks)
  :config
  (bind-key "C-M-l" #'paredit-recentre-on-sexp paredit-mode-map)

  (bind-key ")" #'paredit-close-round-and-newline paredit-mode-map)
  (bind-key "M-)" #'paredit-close-round paredit-mode-map)

  (bind-key "M-k" #'paredit-raise-sexp paredit-mode-map)
  (bind-key "M-I" #'paredit-splice-sexp paredit-mode-map)

  (unbind-key "M-r" paredit-mode-map)
  (unbind-key "M-s" paredit-mode-map)

  (bind-key "C-. D" #'paredit-forward-down paredit-mode-map)
  (bind-key "C-. B" #'paredit-splice-sexp-killing-backward paredit-mode-map)
  (bind-key "C-. C" #'paredit-convolute-sexp paredit-mode-map)
  (bind-key "C-. F" #'paredit-splice-sexp-killing-forward paredit-mode-map)
  (bind-key "C-. a" #'paredit-add-to-next-list paredit-mode-map)
  (bind-key "C-. A" #'paredit-add-to-previous-list paredit-mode-map)
  (bind-key "C-. j" #'paredit-join-with-next-list paredit-mode-map)
  (bind-key "C-. J" #'paredit-join-with-previous-list paredit-mode-map))

(or (use-package mic-paren
      :disabled
      :config
      (paren-activate))
    (use-package paren
      :disabled
      :config
      (show-paren-mode 1)))

(use-package pdf-tools
  :disabled)

(use-package php-mode
  :mode "\\.php\\'")

(use-package projectile
  :commands projectile-mode
  :diminish projectile-mode
  :after helm
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (use-package helm-projectile
    :disabled
    :demand
    :config
    (setq projectile-completion-system 'helm)
    (helm-projectile-toggle 1))
  (projectile-mode)
  (bind-key "s s"
            #'(lambda ()
                (interactive)
                (helm-do-grep-1 (list (projectile-project-root)) t))
            'projectile-command-map))

(use-package python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :config
  (defvar python-mode-initialized nil)

  (defun my-python-mode-hook ()
    (unless python-mode-initialized
      (setq python-mode-initialized t)

      (info-lookup-add-help
       :mode 'python-mode
       :regexp "[a-zA-Z_0-9.]+"
       :doc-spec
       '(("(python)Python Module Index" )
         ("(python)Index"
          (lambda
            (item)
            (cond
             ((string-match
               "\\([A-Za-z0-9_]+\\)() (in module \\([A-Za-z0-9_.]+\\))" item)
              (format "%s.%s" (match-string 2 item)
                      (match-string 1 item)))))))))
    (setq indicate-empty-lines t)
    (set (make-local-variable 'parens-require-spaces) nil)
    (setq indent-tabs-mode nil)

    (bind-key "C-c C-z" #'python-shell python-mode-map)
    (unbind-key "C-c c" python-mode-map))

  (add-hook 'python-mode-hook 'my-python-mode-hook))

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (apply #'hook-into-modes 'rainbow-delimiters-mode lisp-mode-hooks))

(use-package realgud
  :commands (realgud:gdb
             realgud:byebug
             realgud:pry
             realgud:ipdb
             realgud:pdb
             realgud:jdb))

(use-package recentf
  :disabled
  :commands (recentf-mode
             recentf-add-file
             recentf-apply-filename-handlers)
  :preface
  (defun recentf-add-dired-directory ()
    (if (and dired-directory
             (file-directory-p dired-directory)
             (not (string= "/" dired-directory)))
        (let ((last-idx (1- (length dired-directory))))
          (recentf-add-file
           (if (= ?/ (aref dired-directory last-idx))
               (substring dired-directory 0 last-idx)
             dired-directory)))))
  :init
  (add-hook 'dired-mode-hook 'recentf-add-dired-directory)
  :config
  (recentf-mode 1))


(use-package reveal-in-osx-finder
  :if (memq window-system '(mac ns))
  :commands reveal-in-osx-finder)

(use-package rg
  :commands rg
  :if (executable-find "rg"))

(use-package ripgrep
  :commands ripgrep
  :if (executable-find "rg"))

(use-package ruby-mode
  :mode ("\\.rb\\'" . ruby-mode)
  :interpreter ("ruby" . ruby-mode)
  :functions inf-ruby-keys
  :config
  (use-package yari
    :init
    (progn
      (defvar yari-helm-source-ri-pages
        '((name . "RI documentation")
          (candidates . (lambda () (yari-ruby-obarray)))
          (action  ("Show with Yari" . yari))
          (candidate-number-limit . 300)
          (requires-pattern . 2)
          "Source for completing RI documentation."))

      (defun helm-yari (&optional rehash)
        (interactive (list current-prefix-arg))
        (when current-prefix-arg (yari-ruby-obarray rehash))
        (helm 'yari-helm-source-ri-pages (yari-symbol-at-point)))))

  (defun my-ruby-smart-return ()
    (interactive)
    (when (memq (char-after) '(?\| ?\" ?\'))
      (forward-char))
    (call-interactively 'newline-and-indent))

  (defun my-ruby-mode-hook ()
    (require 'inf-ruby)
    (inf-ruby-keys)
    (bind-key "<return>" #'my-ruby-smart-return ruby-mode-map)
    (bind-key "C-h C-i" #'helm-yari ruby-mode-map))

  (add-hook 'ruby-mode-hook 'my-ruby-mode-hook))

(use-package rust-mode
  :mode "\\.rs\\'")

(use-package sass-mode
  :mode "\\.sass\\'")

(use-package scss-mode
  :mode "\\.scss\\'")

(use-package seti-theme
  :disabled
  :demand
  :config (load-theme 'seti))

(use-package sentence-navigation
  :bind (("M-e" . sentence-nav-forward) ("M-a" . sentence-nav-backward)))

(use-package sh-script)

(use-package shell
  :commands (shell shell-mode)
  :init
  ;; shell mode
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (setenv "PAGER" "cat")
  (setenv "TERM" "xterm-256color")

  ;; eshell mode
  ;; (setenv "JAVA_HOME" "/usr/local/java")
  (setenv "EDITOR" "emacsclient -nq")
  (setenv "LC_ALL" "C")
  (setenv "LANG" "en")

  ;;  (defun shell-comint-input-sender-hook ()
  ;;    "Check certain shell commands.
  ;; Executes the appropriate behavior for certain commands."
  ;;    (setq comint-input-sender
  ;;          (lambda (proc command)
  ;;            (cond
  ;;             ;; Check for clear command and execute it.
  ;;             ((string-match "^[ \t]*clear[ \t]*$" command)
  ;;              (comint-send-string proc "\n")
  ;;              (erase-buffer))
  ;;             ;; Check for man command and execute it.
  ;;             ((string-match "^[ \t]*man[ \t]*" command)
  ;;              (comint-send-string proc "\n")
  ;;              (setq command (replace-regexp-in-string
  ;;                             "^[ \t]*man[ \t]*" "" command))
  ;;              (setq command (replace-regexp-in-string
  ;;                             "[ \t]+$" "" command))
  ;;              (funcall 'man command))
  ;;             ;; Send other commands to the default handler.
  ;;             (t (comint-simple-send proc command))))))

  ;;  (add-hook 'shell-mode-hook 'shell-comint-input-sender-hook)
  )

(use-package shell-script-mode
  :commands shell-script-mode
  :ensure nil
  :init
  (add-to-list 'auto-mode-alist '("\\.zsh\\'" . shell-script-mode))
  )

(use-package shell-pop
  :demand)

(use-package skewer-mode
  :commands (skewer-mode skewer-css-mode skewer-html-mode)
  :init
  (add-hook 'js2-mode-hook 'skewer-mode)
  (add-hook 'css-mode-hook 'skewer-css-mode)
  (add-hook 'html-mode-hook 'skewer-html-mode))

(use-package skewer-less
  :commands skewer-less-mode
  :init (add-hook 'less-css-mode-hook 'skewer-less-mode))

(use-package slime
  :commands slime)

(use-package smart-mode-line
  :disabled
  :demand
  :config
  (setq sml/theme 'respectful)
  (sml/setup))

(use-package smart-tabs-mode
  :init
  (add-hook 'prog-mode-hook 'smart-tabs-mode)
  :commands smart-tabs-mode)

(use-package smartparens
  :disabled
  :commands smartparens-mode
  :init
  ;; (require 'smartparens-config)
  ;; (sp-use-paredit-bindings)
  ;; (add-hook 'prog-mode-hook #'smartparens-mode)
  )

(use-package smooth-scrolling
  :disabled
  :commands smooth-scrolling-mode
  :init
  (smooth-scrolling-mode 1))

(use-package solarized-theme
  :disabled
  :demand
  :config
  (load-theme 'solarized-dark))

(use-package spray
  :commands spray-mode)

(use-package swiper
  :bind ("\C-s" . swiper))

(use-package tern
  :commands tern-mode
  :init
  (add-hook 'term-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'js2-mode-hook 'tern-mode))

(use-package tramp)

(use-package try
  :commands try)

(use-package toc-org
  :commands toc-org-enable
  :init
  (add-hook 'org-mode-hook 'toc-org-enable))

(use-package undo-tree
  :config (global-undo-tree-mode 1)
  :bind (("C-c j" . undo-tree-undo)
         ("C-c k" . undo-tree-redo)
         ("C-c l" . undo-tree-switch-branch)
         ("C-c ;" . undo-tree-visualize)))

(use-package web-mode
  :mode "\\.html\\'")

(use-package whitespace
  :defines (whitespace-auto-cleanup
            whitespace-rescan-timer-time
            whitespace-silent)
  :preface
  (defun normalize-file ()
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (whitespace-cleanup)
      (delete-trailing-whitespace)
      (goto-char (point-max))
      (delete-blank-lines)
      (set-buffer-file-coding-system 'unix)
      (goto-char (point-min))
      (while (re-search-forward "\r$" nil t)
        (replace-match ""))
      (set-buffer-file-coding-system 'utf-8)
      (let ((require-final-newline t))
        (save-buffer))))

  (defun maybe-turn-on-whitespace ()
    "Depending on the file, maybe clean up whitespace."
    (let ((file (expand-file-name ".clean"))
          parent-dir)
      (while (and (not (file-exists-p file))
                  (progn
                    (setq parent-dir
                          (file-name-directory
                           (directory-file-name
                            (file-name-directory file))))
                    ;; Give up if we are already at the root dir.
                    (not (string= (file-name-directory file)
                                  parent-dir))))
        ;; Move up to the parent dir and try again.
        (setq file (expand-file-name ".clean" parent-dir)))
      ;; If we found a change log in a parent, use that.
      (when (and (file-exists-p file)
                 (not (file-exists-p ".noclean"))
                 (not (and buffer-file-name
                           (string-match "\\.texi\\'" buffer-file-name))))
        (add-hook 'write-contents-hooks
                  #'(lambda () (ignore (whitespace-cleanup))) nil t)
        (whitespace-cleanup))))

  :init
  (add-hook 'find-file-hooks 'maybe-turn-on-whitespace t)

  :config
  (remove-hook 'find-file-hooks 'whitespace-buffer)
  (remove-hook 'kill-buffer-hook 'whitespace-buffer)

  ;; For some reason, having these in settings.el gets ignored if whitespace
  ;; loads lazily.
  (setq whitespace-auto-cleanup t
        whitespace-line-column 110
        whitespace-rescan-timer-time nil
        whitespace-silent t
        whitespace-style '(face trailing lines space-before-tab empty)))

(use-package whitespace-cleanup-mode
  :commands whitespace-cleanup-mode
  :init
  (add-hook 'prog-mode-hook 'whitespace-cleanup-mode))

(use-package winner
  :disabled
  :if (not noninteractive)
  :bind (("M-N" . winner-redo)
         ("M-P" . winner-undo))
  :config
  (winner-mode 1))

(use-package wrap-region
  :commands wrap-region-mode
  :init
  (add-hook 'prog-mode-hook 'wrap-region-mode)
  :config
  (wrap-region-add-wrappers
   '(("$" "$")
     ("/" "/" nil ruby-mode)
     ("/* " " */" "#" (java-mode javascript-mode css-mode c-mode c++-mode))
     ("`" "`" nil (markdown-mode ruby-mode shell-script-mode)))))

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

(use-package yaml-mode
  :mode "\\.yaml\\'")

(use-package ycmd
  :commands global-ycmd-mode
  :init
  (add-hook 'after-init-hook #'global-ycmd-mode)
  (require 'ycmd-eldoc)
  (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup)
  )

(use-package zenburn-theme
  :disabled
  :demand
  :config (load-theme 'zenburn))

(use-package zonokai-theme
  :disabled
  :demand
  :config (load-theme 'zonokai-blue))

(provide 'packages)
;;; packages.el ends here
