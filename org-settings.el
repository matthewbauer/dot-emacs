(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cfw:read-date-command
   (lambda nil
     (interactive)
     (let
         ((xs
           (decode-time
            (org-time-string-to-time
             (org-read-date)))))
       (list
        (nth 4 xs)
        (nth 3 xs)
        (nth 5 xs)))))
 '(deft-auto-save-interval 0.0)
 '(deft-text-mode (quote org-mode))
 '(org-M-RET-may-split-line (quote ((headline) (default . t))))
 '(org-adapt-indentation nil)
 '(org-agenda-auto-exclude-function (quote org-my-auto-exclude-function))
 '(org-agenda-cmp-user-defined (quote org-compare-todo-age))
 '(org-agenda-deadline-leaders (quote ("!D!: " "D%02d: ")))
 '(org-agenda-default-appointment-duration 60)
 '(org-agenda-fontify-priorities t)
 '(org-agenda-include-diary t)
 '(org-agenda-inhibit-startup t)
 '(org-agenda-log-mode-items (quote (closed clock state)))
 '(org-agenda-ndays 1)
 '(org-agenda-persistent-filter t)
 '(org-agenda-prefix-format
   (quote
    ((agenda . "  %-11c%?-12t% s")
     (timeline . "  % s")
     (todo . "  %-11c%5(org-todo-age) ")
     (tags . "  %-11c"))))
 '(org-agenda-scheduled-leaders (quote ("" "S%d: ")))
 '(org-agenda-scheduled-relative-text "S%d: ")
 '(org-agenda-scheduled-text "")
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-deadline-is-shown t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-skip-unavailable-files t)
 '(org-agenda-sorting-strategy
   (quote
    ((agenda habit-down time-up todo-state-up priority-down)
     (todo priority-down category-keep)
     (tags priority-down category-keep)
     (search category-keep))))
 '(org-agenda-start-on-weekday nil)
 '(org-agenda-start-with-log-mode t)
 '(org-agenda-tags-column -100)
 '(org-agenda-use-time-grid nil)
 '(org-archive-save-context-info (quote (time category itags)))
 '(org-attach-method (quote mv))
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (ditaa . t)
     (haskell . t)
     (calc . t))))
 '(org-beamer-frame-default-options "fragile")
 '(org-clock-clocked-in-display nil)
 '(org-clock-idle-time 10)
 '(org-clock-in-resume t)
 '(org-clock-in-switch-to-state "STARTED")
 '(org-clock-into-drawer "LOGBOOK")
 '(org-clock-mode-line-total (quote current))
 '(org-clock-modeline-total (quote current))
 '(org-clock-out-remove-zero-time-clocks t)
 '(org-clock-out-switch-to-state nil)
 '(org-clock-persist t)
 '(org-clock-resolve-expert t)
 '(org-completion-use-ido t)
 '(org-confirm-elisp-link-function nil)
 '(org-confirm-shell-link-function nil)
 '(org-crypt-disable-auto-save nil)
 '(org-cycle-global-at-bob t)
 '(org-deadline-warning-days 14)
 '(org-drawers (quote ("PROPERTIES" "CLOCK" "LOGBOOK" "OUT")))
 '(org-edit-src-content-indentation 0)
 '(org-enforce-todo-dependencies t)
 '(org-export-babel-evaluate nil)
 '(org-export-latex-classes
   (quote
    (("article" "\\documentclass[11pt]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("linalg" "\\documentclass{article}
\\usepackage{linalgjh}
[DEFAULT-PACKAGES]
[EXTRA]
[PACKAGES]"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[11pt]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[11pt]{book}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("beamer" "\\documentclass{beamer}" org-beamer-sectioning))))
 '(org-extend-today-until 8)
 '(org-fast-tag-selection-single-key (quote expert))
 '(org-fontify-done-headline t)
 '(org-footnote-section nil)
 '(org-habit-preceding-days 42)
 '(org-habit-today-glyph 45)
 '(org-hide-leading-stars t)
 '(org-image-actual-width (quote (800)))
 '(org-insert-heading-respect-content t)
 '(org-irc-link-to-logs t t)
 '(org-mobile-agendas (quote ("Z")))
 '(org-mobile-files-exclude-regexp "\\(TODO\\(-.*\\)?\\)\\'")
'(org-modules (quote (org-gnus org-habit org-info org-depend)))
'(org-priority-faces
(quote
 ((65 :foreground "ForestGreen" :weight bold)
  (66 . "DarkGreen")
  (67 :foreground "dark gray" :slant italic))))
'(org-return-follows-link t)
'(org-reverse-note-order t)
'(org-smart-capture-use-lastname t)
'(org-src-fontify-natively t)
'(org-stuck-projects (quote ("TODO=\"PROJECT\"" nil nil "SCHEDULED:")))
'(org-tags-column -97)
'(org-time-clocksum-use-fractional t)
'(org-todo-keyword-faces
(quote
 (("TODO" :foreground "medium blue" :weight bold)
  ("APPT" :foreground "medium blue" :weight bold)
  ("NOTE" :foreground "brown" :weight bold)
  ("STARTED" :foreground "dark orange" :weight bold)
  ("WAITING" :foreground "red" :weight bold)
  ("DELEGATED" :foreground "dark violet" :weight bold)
  ("DEFERRED" :foreground "dark blue" :weight bold)
  ("SOMEDAY" :foreground "dark blue" :weight bold)
  ("PROJECT" :foreground "#088e8e" :weight bold))))
'(org-todo-repeat-to-state "TODO")
'(org-use-property-inheritance (quote ("AREA")))
'(org-use-speed-commands t)
'(org-use-tag-inheritance nil)
'(org-velocity-always-use-bucket t)
'(org-velocity-exit-on-match t)
'(org-velocity-force-new t)
'(org-velocity-search-method (quote regexp))
'(org-x-backends (quote (ox-org ox-redmine)))
'(org-x-redmine-title-prefix-function (quote org-x-redmine-title-prefix))
'(org-x-redmine-title-prefix-match-function (quote org-x-redmine-title-prefix-match)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-done ((t (:foreground "ForestGreen"))))
 '(org-done ((t (:foreground "ForestGreen" :weight bold))))
 '(org-habit-alert-face ((((background light)) (:background "#f5f946"))))
 '(org-habit-alert-future-face ((((background light)) (:background "#fafca9"))))
 '(org-habit-clear-face ((((background light)) (:background "#8270f9"))))
 '(org-habit-clear-future-face ((((background light)) (:background "#d6e4fc"))))
 '(org-habit-overdue-face ((((background light)) (:background "#f9372d"))))
 '(org-habit-overdue-future-face ((((background light)) (:background "#fc9590"))))
 '(org-habit-ready-face ((((background light)) (:background "#4df946"))))
 '(org-habit-ready-future-face ((((background light)) (:background "#acfca9"))))
 '(org-headline-done ((t nil)))
 '(org-scheduled ((((class color) (min-colors 88) (background light)) nil)))
 '(org-upcoming-deadline ((((class color) (min-colors 88) (background light)) (:foreground "Brown")))))
