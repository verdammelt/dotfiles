(declare-function if-work "init")
(declare-function mjs/bbdb-init "init-bbdb")

(defvar summary-line-format)
(setq summary-line-format "%z%U%R%[%10&user-date;%*%(%1{%-15,15uB%)%}%]%B%s\n")

(defvar rss-summary-line-format)
(setq rss-summary-line-format "%z%U%R%[%10&user-date;%*%ub%(%1{%-15,15f%)%}%*]%B%s\n")

(defvar split-rules)
(setq split-rules
  '(|
    (| (to "noreply@sourceforge.net" "mail.tnef")
       ("subject" "tnef" "mail.tnef"))

    (any ".*@github.com"
         (| ("subject" "exercism/v3" "list.exercism.v3")
            ("subject" "exercism/.*" "list.exercism.maintenance")
            "list.github"))

    (from "notification@slack.com"
          (| ("subject" "Exercism" "list.exercism.slack")
             ("subject" "#maintaining-common-lisp" "list.exercism.slack")
             ("subject" "Def.Method" "defmethod.slack")))

    (| (from "hello@mail.exercism.io" "list.exercism.mentor")
       (from "jeremy@exercism.io" "list.exercism.announce"))

    (from "notifier@codeship.com"
          (| ("subject" "defmethodinc/.*" "defmethod.builds")))

    (from "Auto_Reply@mailer.discoverybenefits.com" "list.bank")

    (| (from ".*@knowyourteam\\.com" "defmethod.misc")
       (from ".*@knowyourcompany\\.com" "defmethod.misc")
       (from ".*@bonus\\.ly" "defmethod.misc")
       (from "helpful@ninety.io" "defmethod.misc")
       (from "noreply@organizationalcheckup.com" "defmethod.misc"))


    ("sender" "calendar-notification@google.com" "defmethod.calendar")

    (| (to "\\(mark\\|msimpson\\)@defmethod\\..*" "defmethod.inbox")
       (to "all@defmethod\\.io" "defmethod.inbox")
       (from ".*@defmethod\\..*" "defmethod.inbox")
       (from "donotreply@adp.com" "defmethod.inbox"))

    (any ".*@LISTSERV.NODAK.EDU" "list.lifelines")
    (from "wsmith@wordsmith.org" "list.awotd")
    (to "extremeprogramming@groups.io" "list.extremeprogramming")
    (to "testdrivendevelopment@groups.io" "list.testdrivendevelopment")
    (to "verdammelt+agiledeveloperspractices@gmail.com" "list.newsletter.tanzer")
    ("subject" "GeePawHill.Org" "list.newsletter.geepawhill")
    (from "robert@stuffwithstuff.com" "list.newsletter.crafting-interpreters")
    (| (from ".*@linkedin.com" "list.misc")
       (from "millionyearpicnic@googlegroups.com" "list.misc")
       (from ".*@acm.org" "list.misc")
       (from "vitalsigns@prohealthcare.com" "list.misc")
       (from "noreply@followmyhealth.com" "list.misc")
       (from ".*@meetup.com" "list.misc")
       (from "noreply@google.com" "list.misc")
       (from "noreply-local-guides@google.com" "list.misc")
       (from "info@fsf.org" "list.misc")
       (from "morbidanatomy@gmail.com" "list.misc")
       (from "no-reply@dropboxmail.com" "list.misc"))

    (| (from "Lyft Ride Receipt <no-reply@lyftmail.com>" "list.receipts")
       (from "t-mobile@digital-delivery.com" "list.receipts")
       (from "receipts@messaging.squareup.com" "list.receipts")
       (from ".*@patreon.com" "list.receipts")
       (from "service@paypal.com" "list.receipts")
       (| (from "Northwellhealth_no-reply@healthpay24.net" "list.receipts")
          (subject "Northwell Health Payment Due Reminder" "list.receipts"))
       (from "orders@starbucks.com" "list.receipts")
       (from "noreply@alerts.psegliny.com" "list.receipts")
       ("subject" "Amazon Web Services Billing Statement Available" "list.receipts")
       (from "shipment-tracking@amazon.com" "list.receipts")
       (from "digital-no-reply@amazon.com" "list.receipts")
       (from "reload-no-reply@amazon.com" "list.receipts")
       (from ".*@etsy.com" "list.receipts")
       (from "googleplay-noreply@google.com" "list.receipts"))

    (| (any "ally.*" "list.bank")
       (any "hsaalerts@avidiahealthcaresolutions.com" "list.bank")
       (any "\*\.optumbank\.com" "list.bank")
       (any ".*mint.*" "list.bank")
       (any ".*citizensbank.*" "list.bank")
       (from "CitizensOneCustomerService@ha.edelivery-view.com" "list.bank")
       (from "webinquiry@Ascensus.com" "list.bank")
       (any ".*@mail.fidelity.com" "list.bank")
       (from "noreply@healthsafe-id.com" "list.bank"))

    (from ".*\.starbucks\.com" "list.starbucks")


    (from "forums@dragonsfoot.org" "list.dragonsfoot")

    (| (from "ArqBackupSystem@virgil.local" "list.arqbackup")
       (from "ArqBackupSystem@virgil.fios-router.home" "list.arqbackup"))

    (any ".*@travis-ci.org" "list.ci-builds")

    ;; apply splitting rules (if any) found in gnus-group-parameters
    (: gnus-group-split-fancy nil t nil)

    ;; split with spam rules
    (: spam-split)

    ;; absolute fallback
    "mail.inbox"))

(defvar group-parameters)
(setq group-parameters
      '(("nnfolder.*"
         (spam-contents gnus-group-spam-classification-ham)
         (spam-process ((ham spam-use-BBDB)))
         (spam-process-destination "nnfolder:spam.spam"))

        ("nnfolder+archive.*"
         (gnus-thread-sort-functions '(gnus-thread-sort-by-number)))
        ("nndraft:.*"
         (gnus-thread-sort-functions '(gnus-thread-sort-by-number)))

        ("mail.*"
         (gcc-self . t)
         (total-expire . t))

        ("mail.codeandcocktails"
         (posting-style  (address "codeandcocktails@gmail.com")))

        ("list.*"
         (total-expire . t))

        ("cyrus.*"
         (gcc-self . t)
         (total-expire . t)
         (posting-style (address "msimpson@cyrusinnovation.com")))

        ("defmethod.*"
         (gcc-self . t)
         (total-expire . t)
         (posting-style (address "msimpson@defmethod.io")))


        ("spam\\.spam"
         (total-expire . t)
         (spam-contents gnus-group-spam-classification-spam)
         (spam-process ((ham spam-use-BBDB)))
         (ham-process-destination "nnfolder:mail.inbox"))

        ("nnfolder+archive.*"
         (total-expire . nil))

        ("^gmane\\."
         (spam-autodetect . t)
         (spam-autodetect-methods spam-use-regex-headers)
         (spam-process (spam spam-use-gmane)))
        ("^gwene\\."
         (gnus-summary-line-format rss-summary-line-format)
         (spam-autodetect . t)
         (spam-autodetect-methods spam-use-regex-headers)
         (spam-process (spam spam-use-gmane)))))

(use-package gnus
  :ensure nil
  :bind (("<f6>" . gnus))
  :mode (("SCORE$" . lisp-mode)
         ("ADAPT$" . lisp-mode))
  :init (setq read-mail-command 'gnus)
  :config
  (mjs/bbdb-init 'gnus)

  (setq gnus-default-directory gnus-directory
        gnus-kill-files-directory
        (expand-file-name "score-files" gnus-directory) ;where to put the kill files
        gnus-message-archive-group
        (if-work "nnfolder:defmethod.inbox" '((format-time-string "archive-%Y")))
        gnus-parameters group-parameters
        gnus-secondary-select-methods '((nnfolder "")) ; where to find my mails
        gnus-select-method '(nntp "news.gmane.io")    ; where to find my news.
        gnus-summary-line-format summary-line-format
        gnus-update-message-archive-method t
        gnus-use-adaptive-scoring '(word line)))

(use-package gnus-art
  :ensure nil
  :config (setq
           gnus-treat-from-gravatar 'head
           gnus-treat-mail-gravatar 'head
           gnus-treat-unsplit-urls t
           gnus-treat-strip-multiple-blank-lines t
           gnus-treat-x-pgp-sig t
           gnus-buttonized-mime-types '("multipart/signed" "multipart/alternative")))

(use-package gnus-dup
  :ensure nil
  :config (setq gnus-save-duplicate-list t))

(use-package gnus-group
  :ensure nil
  :config (setq gnus-group-sort-function
                '(gnus-group-sort-by-alphabet gnus-group-sort-by-rank)))

(use-package gnus-icalendar
  :ensure nil
  :config (gnus-icalendar-setup))

(use-package gnus-msg
  :ensure nil
  :config (setq gnus-gcc-mark-as-read t ; carbon-copies should be auto-read
                gnus-message-replysign t
                gnus-message-replyencrypt t))

(use-package gnus-registry
  :ensure nil
  :init (gnus-registry-initialize)
  :config
  (progn
    (setq gnus-registry-install t
          gnus-registry-split-strategy 'majority
          gnus-registry-max-entries 50000)
    (push '("spam" t) gnus-registry-ignored-groups)))

(use-package gnus-start
  :ensure nil
  :config (setq gnus-init-file (locate-user-emacs-file "init-gnus.el")
                gnus-save-killed-list nil))

(use-package gnus-sum
  :ensure nil
  :init (progn (add-hook 'gnus-summary-exit-hook 'gnus-summary-bubble-group)
               (add-hook 'gnus-summary-exit-hook 'gnus-group-sort-groups-by-rank)
               (add-hook 'gnus-summary-prepared-hook 'gnus-summary-hide-all-threads))
  :config (setq gnus-sum-thread-tree-false-root "< "
                gnus-sum-thread-tree-single-indent "= "
                gnus-user-date-format-alist '(((gnus-seconds-today) . "%H:%M")
                                              (604800 . "%a %H:%M")
                                              ((gnus-seconds-month) . "%a %d")
                                              ((gnus-seconds-year) . "%b %d")
                                              (t . "%Y-%m-%d"))
                gnus-thread-sort-functions '(gnus-thread-sort-by-number
                                             gnus-thread-sort-by-subject
                                             gnus-thread-sort-by-score)
                gnus-subthread-sort-functions  '(gnus-thread-sort-by-number)
                gnus-thread-score-function '+
                gnus-thread-hide-subtree t
                gnus-auto-select-first nil
                gnus-summary-gather-subject-limit 'fuzzy
                gnus-build-sparse-threads nil
                gnus-fetch-old-headers 5000))

(use-package gnus-topic
  :ensure nil
  :hook (gnus-group-mode . gnus-topic-mode)
  :config (setq gnus-topic-display-empty-topics nil))

(use-package mail-source
  :ensure nil
  :config
  (setq
   mail-source-directory (concat gnus-directory "incoming")
   mail-source-primary-source (car mail-sources)
   mail-source-crash-box (concat gnus-directory "crash-box")))

(use-package message
  :ensure nil
  :config (setq message-directory gnus-directory))

(use-package nnfolder
  :ensure nil
  :config (setq nnfolder-directory (concat gnus-directory "mail")))

(use-package nnmail
  :ensure nil
  :functions nnmail-fancy-expiry-targets
  :config
  (setq
   nnmail-treat-duplicates 'delete
   nnmail-cache-accepted-message-ids t
   nnmail-message-id-cache-length 5000
   nnmail-expiry-target 'mjs/expiry-target-calculator
   nnmail-expiry-wait-function 'mjs/expiry-wait-calculator
   nnmail-split-methods 'nnmail-split-fancy
   nnmail-split-fancy split-rules))

(use-package nnreddit)


(use-package spam
  :ensure nil
  :init (spam-initialize)
  :config (progn
            (load (locate-user-emacs-file "lisp/gnus-spam-fixed-bbdb"))
            (setq
             spam-use-spamassassin-headers t  ; because my ISP runs spamassassin
             spam-use-BBDB t
             spam-use-BBDB-exclusive t
             spam-mark-ham-unread-before-move-from-spam-group t ; ham moved from spam folders will be marked unread.
             spam-split-group "spam.spam"
             )))

(use-package gnus-score
  :ensure nil
  :init (progn (add-hook 'message-sent-hook 'gnus-score-followup-article)
               (add-hook 'message-sent-hook 'gnus-score-followup-thread))
  :config
  (progn
    (setq gnus-decay-scores t  ; temporary scores should degrade over time.
          gnus-score-find-score-files-function 'gnus-score-find-hierarchical
          gnus-adaptive-pretty-print t
          gnus-adaptive-word-no-group-words t
          gnus-score-thread-simplify t)
    (add-to-list 'gnus-default-adaptive-score-alist
                 '(gnus-ticked-mark (subject 10)))))

;; TODO: exeripment with the need for this settings.
;; (use-package shr
;;   :ensure nil
;;   :config
;;   ;; Improve colors used in shr. These settings are to make sure colors
;;   ;; are distinct enough to be visible.
;;   (use-package shr-color
;;     :ensure nil
;;     :config
;;     (setq shr-color-visible-distance-min 40
;;           shr-color-visible-luminance-min 70)))

(use-package mm-decode
  :ensure nil
  :config
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

(defun gnus-summary-sort-by-total-score (&optional reverse)
         (interactive)
         (gnus-summary-sort 'total-score reverse))

(defun mjs/average-score (&rest scores)
  (/ (apply #'+ scores) (length scores)))

(defun mjs/expiry-wait-calculator (group)
  (let ((wait-days
         (cond ((string-match "archive" group) 'never)
               ((string-match "spam" group) 1)
               ((string-match "list\\.*" group) 14)
               ((string-match "tnef" group) 'never)
               ((string-match "codeandcocktails" group) 'never)
               (t 28))))
    (message "expiry-wait for %s is %s" group wait-days)
    wait-days))

(defun mjs/expiry-target-calculator (group)
  (let* ((expiry-target-file
          (cond ((string-match "list\\.*" group) 'delete)
                ((string-match "spam\\.*" group) 'delete)
                ((string-match "mail\\.misc" group) 'delete)
                ((string-match "defmethod\\.inbox" group) "nnfolder+archive:defmethod.archive-%Y")
                ((string-match "defmethod\\.*" group) 'delete)
                (t "nnfolder+archive:archive-%Y"))))
    (message "expiry-target for %s is '%s'" group expiry-target-file)
    (if (stringp expiry-target-file)
        (let ((nnmail-fancy-expiry-targets `(("from" ".*" ,expiry-target-file))))
          (nnmail-fancy-expiry-target group))
      expiry-target-file)))
