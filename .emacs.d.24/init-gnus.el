;;;;
;;;; .gnus.el
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Time-stamp: <2011-12-16 21:04:52 mark>
;;;;
;;;
;;; TODO: 
;;; * summary/modelines formats 
;;; * more splitting
;;; * scoring
;;; * sorting articles
;;; * sorting groups
;;;

;;; load special version of spam.el to get fix for bbdb bug
(load "~/SRC/gnus/lisp/spam.el")

;;; 
;;; Select methods
;;;
(setq gnus-select-method '(nntp "news.gmane.org") ; where to find my news.
 gnus-secondary-select-methods '((nnfolder ""))) ; where to find my mails

;;; 
;;; General settings
;;;
(setq 
 gnus-decay-scores t	  ; temporary scores should degrade over time.
 gnus-kill-files-directory (expand-file-name "score-files" gnus-directory) ;where to put the kill files
 gnus-gcc-mark-as-read t	   ; carbon-copies should be auto-read

 ;; formatting the screen
 gnus-summary-line-format "%10&user-date; %U%R%z%I%(%[%4L: %-20,20uB%]%)%O%s\n"
 gnus-user-date-format-alist '(((gnus-seconds-today) . "%H:%M") ;change date display depending upon age of article 
			       (604800 . "%a %H:%M")
			       ((gnus-seconds-month) . "%a %d")
			       ((gnus-seconds-year) . "%b %d")
			       (t . "%Y-%m-%d"))

 ;; sorting of messages
 gnus-thread-sort-functions '(gnus-thread-sort-by-number gnus-thread-sort-by-total-score)

 ;; archiving
 gnus-update-message-archive-method t	;always update archive method - let's us change it quickly

 ;; posting style - what my return address is:
 gnus-posting-styles  '(("nnfolder:.*"
			 (From (with-current-buffer gnus-article-buffer 
				 (message-fetch-field "to")))))
 )

;;; 
;;; Registry
;;;
(setq
 gnus-registry-install t		; yes we use the registry
 gnus-registry-split-strategy 'majority ; splitting to the place that gets the most 'votes'
 )
(gnus-registry-initialize)		; fire up the registry

;;;
;;; Mail
;;;
(setq
 message-directory gnus-directory	; where mail is located
 nnfolder-directory (concat gnus-directory "mail")
 mail-source-directory (concat gnus-directory "incoming") ; where the mail is located
 mail-source-primary-source (car mail-sources) ;check for new mail
 mail-source-crash-box (concat gnus-directory "crash-box")
 )

;;;
;;; Spam
;;;
(setq
 spam-use-spamassassin-headers t    ; because my ISP runs spamassassin
 spam-use-bogofilter t		    ; I want to fine tune the spam checking with local bogofilter
 spam-use-BBDB t
 spam-use-BBDB-exlusive t
 spam-mark-ham-unread-before-move-from-spam-group t ; ham moved from spam folders will be marked unread.
 )
(spam-initialize)

;;;
;;; Splitting
;;;
(setq
 nnmail-split-methods 'nnmail-split-fancy
 nnmail-split-fancy '(| (: spam-split)
			(: gnus-group-split-fancy)
			"mail.inbox")
 spam-split-group "spam.spam"
 )
(load (locate-user-emacs-file "lisp/gnus-group-split-fancy"))		; patched version to reads from gnus-parameters correctly

;;;
;;; Scoring
;;;
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)
(setq gnus-use-adaptive-scoring t)

;;;
;;; Expiry
;;;
;;; expire mail to an archive mailspool for the year.
;;;
(setq 
 nnmail-expiry-target 'nnmail-fancy-expiry-target
 nnmail-fancy-expiry-target '(("from" ".*" "nnfolder+archive:archive-%Y")))

;;; 
;;; Group Parameters
;;;
(setq gnus-parameters
      '(("nnfolder.*"
	 (spam-contents gnus-group-spam-classification-ham)
	 (spam-process ((spam spam-use-bogofilter)
			(ham spam-use-bogofilter)
			(ham spam-use-BBDB)))
	 (spam-process-destination "nnfolder:spam.spam"))

	("mail.*"
	 (total-expire . t))

	("list.*"
	 (total-expire . t)
	 (expiry-target . delete))

	("list\.awotd"
	 (extra-aliases "wsmith@wordsmith.org"))
	("list\.bank"
	 (extra-aliases "email@transfers.ally.com"
			"SchwabAlerts.AccountActivity@schwab.com"
			"online.service@schwab.com"))
	("list\.baznex"
	 (to-address . "baznex@googlegroups.com"))
	("list\.bikes"
	 (extra-aliases "bikeinfo@massbike.org"
			"charlie@livablestreets.info"))
	("list\.dailylit"
	 (extra-aliases "books@dailylit.com"))
	("list\.misc"
	 (extra-aliases "no-reply@posterous.com"
			"info@meetup.com"))

	("spam\.spam"
	 (total-expire . t)
	 (expiry-wait . 1)
	 (expiry-target . delete)
	 (spam-contents gnus-group-spam-classification-spam)
	 (spam-process ((spam spam-use-bogofilter)
			(ham spam-use-bogofilter)
			(ham spam-use-BBDB)))
	 (ham-process-destination "nnfolder:mail.inbox"))

	("^gmane\."
	 (spam-autodetect . t)
	 (spam-autodetect-methods spam-use-regex-headers)
	 (spam-process (spam spam-use-gmane)))))

;;;
;;; DEMONS
;;;
(gnus-demon-add-scan-timestamps)	; setting timestamps
(gnus-demon-add-scanmail)		; get the new mails
(gnus-demon-init)			; poke the daemon to get it going

;;;
;;; utility items
;;;
(add-to-list 'auto-mode-alist '("SCORE$" . lisp-mode))
(add-to-list 'auto-mode-alist '("ADAPT$" . lisp-mode))

(gnus-compile)		  ; doc claims that this will speed things up.
