(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(check-mail-boxes (quote ("~/Messages/incoming/mail\\..*\\.spool")))
 '(check-mail-summary-function (quote check-mail-box-summary))
 '(gnus-always-read-dribble-file t)
 '(gnus-article-prepare-hook nil)
 '(gnus-article-sort-functions (quote (gnus-article-sort-by-score)))
 '(gnus-check-new-newsgroups nil)
 '(gnus-default-article-saver (quote gnus-summary-save-in-folder))
 '(gnus-group-goto-unread nil)
 '(gnus-inhibit-startup-message t)
 '(gnus-read-newsrc-file nil)
 '(gnus-refer-article-method (quote (current (nnweb "refer" (nnweb-type google)))))
 '(gnus-save-killed-list nil)
 '(gnus-save-newsrc-file nil)
 '(gnus-select-group-hook
   (quote
    ((lambda nil
       (mapcar
        (lambda
          (header)
          (mail-header-set-subject header
                                   (gnus-simplify-subject
                                    (mail-header-subject header)
                                    (quote re-only))))
        gnus-newsgroup-headers))
     gnus-group-set-timestamp)))
 '(gnus-single-article-buffer t)
 '(gnus-summary-exit-hook (quote (gnus-summary-bubble-group)))
 '(gnus-summary-ignore-duplicates t)
 '(gnus-thread-sort-functions
   (quote
    ((not gnus-thread-sort-by-number)
     gnus-thread-sort-by-score)))
 '(gnus-treat-body-boundary (quote head))
 '(gnus-treat-display-x-face (quote head))
 '(gnus-treat-from-gravatar (quote head))
 '(gnus-treat-mail-gravatar (quote head))
 '(gnus-use-cache (quote passive))
 '(gnus-use-trees t)
 '(gnus-widen-article-window t)
 '(gnus-activate-level 2)
 '(gnus-after-getting-new-news-hook
   (quote
    (gnus-group-list-groups gnus-group-save-newsrc gnus-display-time-event-handler)))
 '(gnus-agent-expire-all t)
 '(gnus-agent-expire-days 14)
 '(gnus-agent-go-online t)
 '(gnus-agent-mark-unread-after-downloaded nil)
 '(gnus-agent-synchronize-flags t)
 '(gnus-alias-allow-forward-as-reply t)
 '(gnus-alias-default-identity "Gmail")
 '(gnus-alias-override-user-mail-address t)
 '(gnus-alias-unknown-identity-rule (quote error))
 '(gnus-always-read-dribble-file t)
 '(gnus-article-date-lapsed-new-header t)
 '(gnus-article-sort-functions
   (quote
    (gnus-article-sort-by-score gnus-article-sort-by-number)))
 '(gnus-article-update-date-headers nil)
 '(gnus-asynchronous t)
 '(gnus-check-new-newsgroups nil)
 '(gnus-completing-read-function (quote gnus-ido-completing-read))
 '(gnus-default-adaptive-score-alist
   (quote
    ((gnus-saved-mark
      (subject 250)
      (from 50))
     (gnus-dormant-mark
      (subject 150)
      (from 50))
     (gnus-forwarded-mark
      (subject 100)
      (from 25))
     (gnus-replied-mark
      (subject 75)
      (from 15))
     (gnus-ticked-mark
      (subject 0)
      (from 0))
     (gnus-read-mark
      (subject 30)
      (from 5))
     (gnus-del-mark
      (subject 5)
      (from 0))
     (gnus-recent-mark
      (subject 0)
      (from 0))
     (gnus-killed-mark
      (subject -5)
      (from -5))
     (gnus-catchup-mark
      (subject -150)
      (from 0))
     (gnus-duplicate-mark
      (subject -150)
      (from 0))
     (gnus-expirable-mark
      (subject -250)
      (from 0))
     (gnus-spam-mark
      (subject -10)
      (from -150)))))
 '(gnus-default-article-saver (quote gnus-summary-save-in-mail))
 '(gnus-gcc-mark-as-read t)
 '(gnus-generate-tree-function (quote gnus-generate-horizontal-tree))
 '(gnus-group-default-list-level 2)
 '(gnus-group-line-format "%S%p%P%M%5y: %(%B%G%B%)
")
 '(gnus-group-mode-hook (quote (gnus-topic-mode gnus-agent-mode hl-line-mode)))
 '(gnus-group-use-permanent-levels t)
 '(gnus-harvest-sender-alist (quote ((".*@gnu\\.org" . johnw@gnu\.org))))
 '(gnus-home-directory "~/Messages/Gnus/")
 '(gnus-ignored-mime-types
   (quote
    ("application/x-pkcs7-signature" "application/ms-tnef" "text/x-vcard")))
 '(gnus-interactive-exit (quote quiet))
 '(gnus-large-newsgroup 4000)
 '(gnus-mailing-list-groups "\\`\\(list\\|wg21\\)\\.")
 '(gnus-mark-unpicked-articles-as-read t)
 '(gnus-message-archive-group (quote ((format-time-string "sent.%Y"))))
 '(gnus-novice-user nil)
 '(gnus-permanently-visible-groups "INBOX")
 '(gnus-read-active-file nil)
 '(gnus-read-newsrc-file nil)
 '(gnus-refer-article-method
   (quote
    (current
     (nnir "nnimap:Local")
     (nntp "Gmane"
           (nntp-address "news.gmane.org"))
     )))
 '(gnus-registry-ignored-groups (quote (("nntp" t) ("^INBOX" t))))
 '(gnus-inhibit-startup-message t)
 '(gnus-interactive-exit (quote quiet))
 '(gnus-save-killed-list nil)
 '(gnus-save-newsrc-file nil)
 '(gnus-score-default-duration (quote p))
 '(gnus-score-expiry-days 30)
 '(gnus-score-interactive-default-score 10)
 '(gnus-secondary-select-methods
   (quote
    ((nntp "gwene" (nntp-address "news.gwene.org"))
     (nnimap "gmail"
             (nnimap-address "imap.gmail.com")
             (nnimap-server-port 993)
             (nnimap-stream ssl)
             (nnir-search-engine imap)
             ))))
 '(gnus-secondary-servers (quote ("news.gmane.org")))
 '(gnus-select-method (quote (nntp "news.gmane.org")))
 '(gnus-select-group-hook (quote (gnus-group-set-timestamp)))
 '(gnus-sieve-select-method "nnimap:Local")
 '(gnus-signature-separator (quote ("^-- $" "^-- *$" "^_____+$")))
 '(gnus-simplify-subject-functions (quote (gnus-simplify-subject-fuzzy)))
 '(gnus-sort-gathered-threads-function (quote gnus-thread-sort-by-date) t)
 '(gnus-split-methods
   (quote
    ((gnus-save-site-lisp-file)
     (gnus-article-archive-name)
     (gnus-article-nndoc-name))))
 '(gnus-started-hook
   (quote
    ((lambda nil
       (run-hooks
        (quote gnus-after-getting-new-news-hook))))))
 '(gnus-subscribe-newsgroup-method (quote gnus-subscribe-topics))
 '(gnus-sum-thread-tree-single-indent "  ")
 '(gnus-summary-expunge-below -100)
 '(gnus-summary-line-format "%«%3t %U%R %uS %ur %»%(%*%-14,14f   %1«%B%s%»%)
")
 '(gnus-summary-mark-below -100)
 '(gnus-summary-pick-line-format "%U%R %uS %ur %(%*%-14,14f  %B%s%)
")
 '(gnus-summary-prepared-hook (quote (gnus-summary-hide-all-threads)))
 '(gnus-summary-save-parts-default-mime ".*")
 '(gnus-suppress-duplicates t)
 '(gnus-suspend-gnus-hook (quote (gnus-group-save-newsrc)))
 '(gnus-thread-expunge-below -1000)
 '(gnus-thread-hide-subtree t)
 '(gnus-thread-ignore-subject nil)
 '(gnus-thread-score-function (quote max))
 '(gnus-view-pseudo-asynchronously t)
 '(gnus-thread-sort-functions
   (quote
    (gnus-thread-sort-by-total-score gnus-thread-sort-by-most-recent-number)))
 '(gnus-topic-display-empty-topics nil)
 '(gnus-topic-line-format "%i[ %A: %(%{%n%}%) ]%v
")
 '(gnus-treat-date-lapsed (quote head))
 '(gnus-treat-hide-citation-maybe t)
 '(gnus-treat-strip-cr t)
 '(gnus-treat-strip-leading-blank-lines t)
 '(gnus-treat-strip-multiple-blank-lines t)
 '(gnus-treat-strip-trailing-blank-lines t)
 '(gnus-treat-unsplit-urls t)
 '(gnus-tree-minimize-window nil)
 '(gnus-uncacheable-groups "^nnml")
 '(gnus-use-adaptive-scoring (quote (line)))
 '(gnus-use-cache t)
 '(gnus-verbose 4)
 '(gnus-article-date-headers (quote (local)))
 '(gnus-gcc-mark-as-read t)
 '(mail-envelope-from (quote header))
 '(mail-setup-with-from nil)
 '(mail-source-delete-incoming t)
 '(mail-source-delete-old-incoming-confirm nil)
 '(mail-source-report-new-mail-interval 15)
 '(mail-specify-envelope-from t)
 '(mail-user-agent (quote gnus-user-agent))
 '(message-directory "~/Messages/Gnus/Mail/")
 '(message-fill-column 78)
 '(message-interactive t)
 '(message-mail-alias-type nil)
 '(message-mode-hook
   (quote
    (abbrev-mode footnote-mode turn-on-auto-fill turn-on-flyspell
                 (lambda nil
                   (set-fill-column 78))
                 turn-on-orgstruct++ turn-on-orgtbl)))
 '(message-send-mail-function (quote message-send-mail-with-sendmail))
 '(message-send-mail-partially-limit nil)
 '(message-sendmail-envelope-from (quote header))
 '(message-sent-hook (quote (my-gnus-score-followup)))
 '(message-setup-hook (quote (gnus-harvest-set-from message-check-recipients)))
 '(message-signature-separator "^-- *$")
 '(message-subscribed-address-functions (quote (gnus-find-subscribed-addresses)))
 '(message-x-completion-alist
   (quote
    (("\\([rR]esent-\\|[rR]eply-\\)?[tT]o:\\|[bB]?[cC][cC]:" . gnus-harvest-find-address)
     ((if
          (boundp
           (quote message-newgroups-header-regexp))
          message-newgroups-header-regexp message-newsgroups-header-regexp)
      . message-expand-group))))
 '(mm-attachment-override-types
   (quote
    ("text/x-vcard" "application/pkcs7-mime" "application/x-pkcs7-mime" "application/pkcs7-signature" "application/x-pkcs7-signature" "image/.*")))
 '(mm-decrypt-option (quote always))
 '(mm-discouraged-alternatives (quote ("application/msword" "text/richtext" "text/html")))
 '(mm-inline-text-html-with-images nil)
 '(mm-verify-option (quote always))
 '(mm-w3m-safe-url-regexp nil)
 '(nnir-imap-default-search-key "imap")
 '(nnmail-crosspost nil)
 '(nnmail-expiry-wait 30)
 '(nnmail-extra-headers (quote (To Cc Newsgroups)))
 '(nnmail-scan-directory-mail-source-once t)
 '(sc-citation-leader "")
 '(sc-confirm-always-p nil)
 '(sc-default-attribution "")
 '(sc-default-cite-frame
   (quote
    ((begin
      (progn
        (sc-fill-if-different)
        (setq sc-tmp-nested-regexp
              (sc-cite-regexp "")
              sc-tmp-nonnested-regexp
              (sc-cite-regexp)
              sc-tmp-dumb-regexp
              (concat "\\("
                      (sc-cite-regexp "")
                      "\\)"
                      (sc-cite-regexp sc-citation-nonnested-root-regexp)))))
     ("^[   ]*$"
      (if sc-cite-blank-lines-p
          (sc-cite-line)
        (sc-fill-if-different "")))
     ((and
       (looking-at "^-- ?$")
       (not
        (save-excursion
          (goto-char
           (match-end 0))
          (re-search-forward "^-- ?$" nil t))))
      (sc-fill-if-different ""))
     (sc-reference-tag-string
      (if
          (string= sc-reference-tag-string "")
          (list
           (quote continue))
        nil))
     (sc-tmp-dumb-regexp
      (sc-cite-coerce-dumb-citer))
     (sc-tmp-nested-regexp
      (sc-add-citation-level))
     (sc-tmp-nonnested-regexp
      (sc-cite-coerce-cited-line))
     (sc-nested-citation-p
      (sc-add-citation-level))
     (t
      (sc-cite-line))
     (end
      (sc-fill-if-different "")))))
 '(sc-preferred-attribution-list (quote ("initials")))
 '(sc-use-only-preference-p t)
 '(send-mail-function (quote sendmail-send-it))
 '(smtpmail-default-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 587)
 '(smtpmail-stream-type (quote ssl))
 '(spam-report-gmane-use-article-number nil)
 '(spam-use-regex-headers t)
 '(spam-use-spamassassin t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(message-cited-text ((((class color)) (:foreground "Blue"))))
 '(message-header-cc ((((class color)) (:bold t :foreground "green2"))))
 '(message-header-name ((((class color)) (:bold nil :foreground "Blue"))))
 '(message-header-other ((((class color)) (:foreground "Firebrick"))))
 '(message-header-xheader ((((class color)) (:foreground "Blue"))))
 '(message-mml ((((class color)) (:foreground "DarkGreen"))))
 '(message-separator ((((class color)) (:foreground "Tan")))))
