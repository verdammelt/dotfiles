;;;;
;;;; GNUS
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;
;;; TODO:
;;; * sent mail goes to inbox.
;;; * more splitting
;;;

;;;
;;; Select methods
;;;
(with-eval-after-load 'gnus
  (require 'bbdb)

  (setq gnus-init-file (locate-user-emacs-file "init-gnus.el"))

  (setq
   gnus-select-method '(nntp "news.gmane.org") ; where to find my news.
   gnus-secondary-select-methods '((nnfolder ""))) ; where to find my mails

  (setq
   gnus-treat-from-gravatar 'head
   gnus-treat-mail-gravatar 'head
   gnus-treat-unsplit-urls t
   gnus-treat-strip-multiple-blank-lines t
   gnus-treat-x-pgp-sig t
   )
;;
;;; General settings
;;;
  (setq
   gnus-gcc-mark-as-read t	   ; carbon-copies should be auto-read

   gnus-save-killed-list nil

   gnus-default-directory gnus-directory

   ;; formatting the screen
   gnus-summary-line-format
   "%5V%U%R%[%10&user-date;%*%(%1{%-21,21uB%)%}%]%B%s\n"

   gnus-sum-thread-tree-false-root "< "
   gnus-sum-thread-tree-single-indent "= "

   ;;change date display depending upon age of article
   gnus-user-date-format-alist '(((gnus-seconds-today) . "%H:%M")
                                 (604800 . "%a %H:%M")
                                 ((gnus-seconds-month) . "%a %d")
                                 ((gnus-seconds-year) . "%b %d")
                                 (t . "%Y-%m-%d"))

   ;; archiving - always update (lets us change it quickly)
   gnus-update-message-archive-method t
   )

;;;
;;; Sorting Group List
;;;
  (setq gnus-group-sort-function
        '(gnus-group-sort-by-alphabet gnus-group-sort-by-rank))
  (add-hook 'gnus-summary-exit-hook 'gnus-summary-bubble-group)
  (add-hook 'gnus-summary-exit-hook 'gnus-group-sort-groups-by-rank)

;;;
;;; Threading
;;;
  (setq
   gnus-thread-sort-functions '(gnus-thread-sort-by-number
                                gnus-thread-sort-by-subject
                                gnus-thread-sort-by-total-score)
   gnus-sort-gathered-threads-function 'gnus-thread-sort-by-date
   gnus-thread-score-function 'max
   gnus-thread-hide-subtree t
   gnus-auto-select-first nil
   gnus-summary-gather-subject-limit 'fuzzy
   gnus-build-sparse-threads nil
   gnus-fetch-old-headers 5000
   )

;;;
;;; Registry
;;;
  (setq gnus-registry-install t
        gnus-registry-split-strategy 'majority
        gnus-registry-max-entries 50000)
  (gnus-registry-initialize)

;;;
;;; Mail
;;;
  (setq
   message-directory gnus-directory	; where mail is located
   nnfolder-directory (concat gnus-directory "mail")
   mail-source-directory (concat gnus-directory "incoming")
   mail-source-primary-source (car mail-sources)
   mail-source-crash-box (concat gnus-directory "crash-box")

   gnus-save-duplicate-list t
   nnmail-treat-duplicates 'delete
   nnmail-cache-accepted-message-ids t
   nnmail-message-id-cache-length 5000
   )

;;;
;;; Spam
;;;
  (load (locate-user-emacs-file "lisp/gnus-spam-fixed-bbdb"))
  (setq
   spam-use-spamassassin-headers t  ; because my ISP runs spamassassin
   spam-use-BBDB t
   spam-use-BBDB-exclusive t
   spam-mark-ham-unread-before-move-from-spam-group t ; ham moved from spam folders will be marked unread.
   )
  (spam-initialize)

;;;
;;; Splitting
;;;
  (setq
   nnmail-split-methods 'nnmail-split-fancy
   spam-split-group "spam.spam"
   nnmail-split-fancy '(|
                        ("subject" "Message left on server:.*" "mail.misc")
                        (to "codeandcocktails@gmail.com" "mail.codeandcocktails")
                        (to "clojure-dev@googlegroups.com" "list.clojure-dev")
                        (to "noreply@sourceforge.net" "mail.tnef")
                        (|
                         (from "nomail@mobicorp.com" "cyrus.mobi-jenkins")
                         (from "noreply-mobiDevelopment@mobicorp.com" "cyrus.mobi-jira")
                         (from "confluence@pointserve.com" "cyrus.mobi-jira")
                         (from ".*@mobicorp.com" "cyrus.mobi"))
                        (|
                         (to "msimpson+aetna@cyrusinnovation.com" "cyrus.aetna")
                         (any ".*@aetna.com" "cyrus.aetna")
                         (any ".*@itriagehealth.com" "cyrus.aetna"))
                        (| (to "msimpson@cyrusinnovation.com" "cyrus.inbox")
                           (any ".*cyrusinnovation.com" "cyrus.inbox")
                           (any ".*cyruslists.com" "cyrus.inbox"))
                        (to "discuss-bawch@googlegroups.com" "list.bawch")
                        (to "clean-code-discussion@googlegroups.com" "list.cleancode")
                        (from "wsmith@wordsmith.org" "list.awotd")
                        (| (any "ally.*" "list.bank")
                           (any ".*mint.*" "list.bank")
                           (any ".*citizensbank.*" "list.bank")
                           (any ".*@mail.fidelity.com" "list.bank"))
                        (to "boston-software-crafstmanship@googlegroups.com" "list.boston-software-crafstmanship")
                        (from "books@dailylit.com" "list.dailylit")
                        (| (any "ELine@cambridgema.gov" "list.misc")
                           (any "info@harvard.com" "list.misc")
                           (any ".*zipcarmail.com" "list.misc"))
                        (any ".*@travis-ci.org" "list.ci-builds")
                        (any ".*@github.com" "list.github")
                        (any ".*@exercism.io" "list.exercism")
                        (| (any ".*flickr" "list.social-media")
                           (any ".*facebookmail" "list.social-media")
                           (any ".*twitter" "list.social-media")
                           (any ".*linkedin" "list.social-media")
                           (any ".*@stackexchange.com" "list.social-media")
                           (any ".*@postcrossing.com" "list.social-media")
                           (any ".*@meetup.com" "list.social-media"))

                        ;; (: gnus-registry-split-fancy-with-parent)
                        (: gnus-group-split-fancy nil t nil)
                        (: spam-split)

                        "mail.inbox"))

;;;
;;; Scoring
;;;
  (with-eval-after-load 'gnus-score
    (add-hook 'message-sent-hook 'gnus-score-followup-article)
    (add-hook 'message-sent-hook 'gnus-score-followup-thread)
    (setq
     gnus-decay-scores t	  ; temporary scores should degrade over time.
     gnus-kill-files-directory (expand-file-name "score-files" gnus-directory) ;where to put the kill files
     gnus-use-adaptive-scoring t
     gnus-score-find-score-files-function
     '(gnus-score-find-hierarchical
       (lambda (group) '("SCORE")))
     gnus-adaptive-pretty-print t
     gnus-adaptive-word-no-group-words t
     gnus-score-thread-simplify t
     )
    (add-to-list 'gnus-default-adaptive-score-alist
                 '(gnus-ticked-mark (subject 10))))

;;;
;;; Expiry
;;;
;;; expire mail to an archive mailspool for the year.
;;;
  (defun mjs/expiry-wait-calculator (group)
    (let ((wait-days
           (cond ((string-match "spam" group) 1)
                 ((string-match "list\\.*" group) 14)
                 ((string-match "tnef" group) 'never)
                 ((string-match "codeandcocktails" group) 'never)
                 ((string-match "tracker-tool" group) 2)
                 (t 28))))
      (message "expiry-wait for %s is %s" group wait-days)
      wait-days))

  (defun mjs/expiry-target-calculator (group)
    (let* ((expiry-target-file
            (cond ((string-match "list\\.*" group) 'delete)
                  ((string-match "spam\\.*" group) 'delete)
                  ((string-match "mail.misc" group) 'delete)
                  ((string-match "cyrus\\.tracker-tool" group) 'delete)
                  ((string-match "cyrus\\.inbox" group) "nnfolder+archive:cyrus.archive-%Y")
                  ((string-match "cyrus\\.aetna" group) "nnfolder+archive:cyrus.aetna-archive-%Y")
                  ((string-match "cyrus\\.mobi" group) "nnfolder+archive:cyrus.mobi-archive-%Y")
                  ((string-match "cyrus\\.voxy" group) "nnfolder+archive:cyrus.voxy-archive-%Y")
                  (t "nnfolder+archive:archive-%Y"))))
      (message "expiry-target for %s is '%s'" group expiry-target-file)
      (if (stringp expiry-target-file)
          (let ((nnmail-fancy-expiry-targets `(("from" ".*" ,expiry-target-file))))
            (nnmail-fancy-expiry-target group))
        expiry-target-file)))

  (setq
   gnus-message-archive-group '((format-time-string "archive-%Y"))
   nnmail-expiry-target 'mjs/expiry-target-calculator
   nnmail-expiry-wait-function 'mjs/expiry-wait-calculator
   )

;;;
;;; Group Parameters
;;;
  (setq gnus-parameters
        '(("nnfolder.*"
           (spam-contents gnus-group-spam-classification-ham)
           (spam-process ((ham spam-use-BBDB)))
           (spam-process-destination "nnfolder:spam.spam"))

          ("mail.*"
           (gcc-self . t)
           (total-expire . t))

          ("mail.codeandcocktails"
           (posting-style  (address "codeandcocktails@gmail.com")))

          ("list.*"
           (total-expire . t)
           )

          ("cyrus.*"
           (gcc-self . t)
           (total-expire . t)
           (posting-style (address "msimpson@cyrusinnovation.com")))

          ("spam\.spam"
           (total-expire . t)
           ;; (expiry-target . delete)
           (spam-contents gnus-group-spam-classification-spam)
           (spam-process ((ham spam-use-BBDB)))
           (ham-process-destination "nnfolder:mail.inbox"))

          ("^gmane\."
           (spam-autodetect . t)
           (spam-autodetect-methods spam-use-regex-headers)
           (spam-process (spam spam-use-gmane)))))

;;;
;;; utility items
;;;
  (add-to-list 'auto-mode-alist '("SCORE$" . lisp-mode))
  (add-to-list 'auto-mode-alist '("ADAPT$" . lisp-mode)))


(global-set-key (kbd "<f6>") 'gnus)

(setq read-mail-command 'gnus)

;; Improve colors used in shr. These settings are to make sure colors
;; are distinct enough to be visible.
(with-eval-after-load 'shr
  (setq shr-color-visible-distance-min 10
        shr-color-visible-luminance-min 60))

(setq gnus-buttonized-mime-types '("multipart/signed"))

;; encrypt/decrypt/signing
(setq gnus-message-replysign t
      gnus-message-replyencrypt t)
