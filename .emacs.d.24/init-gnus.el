;;;
;;;; .gnus.el
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Time-stamp: <2011-12-08 00:01:19 mark>
;;;;

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
 ;; general
 message-directory gnus-directory	; where mail is located
 nnfolder-directory (concat gnus-directory "mail")
 mail-source-directory (concat gnus-directory "incoming") ; where the mail is located
 mail-source-primary-source (car mail-sources) ;check for new mail
 mail-source-crash-box (concat gnus-directory "crash-box")

 ;; formatting the screen
 gnus-summary-line-format "%10&user-date; %U%R%z%I%(%[%4L: %-20,20uB%]%)%O%s\n"
 gnus-user-date-format-alist '(((gnus-seconds-today) . "%H:%M")
			       (604800 . "%a %H:%M")
			       ((gnus-seconds-month) . "%a %d")
			       ((gnus-seconds-year) . "%b %d")
			       (t . "%Y-%m-%d"))


 ;; sorting of messages
 gnus-thread-sort-functions '(gnus-thread-sort-by-number gnus-thread-sort-by-total-score)

 ;; archiving
 gnus-update-message-archive-method t

 ;; spam
 spam-use-spamassassin-headers t    ; because my ISP runs spamassassin
 spam-use-bogofilter t		    ; I want to fine tune the spam checking with local bogofilter
 spam-mark-ham-unread-before-move-from-spam-group t ; ham moved from spam folders will be marked unread.

 ;; splitting
 nnmail-split-methods 'nnmail-split-fancy
 nnmail-split-fancy '(| (: spam-split)
			"mail.misc")
 spam-split-group "spam.spam"
 )
(spam-initialize)

;;;
;;; Scoring
;;;
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)
(setq gnus-use-adaptive-scoring t)

;;; 
;;; Group Parameters
;;;
(setq gnus-parameters
      '(("nnfolder.*"
	 (spam-contents gnus-group-spam-classification-ham)
	 (spam-process ((spam spam-use-bogofilter)
			(ham spam-use-bogofilter)
			(spam spam-use-BBDB)
			(ham spam-use-BBDB)))
	 (spam-process-destination "nnfolder:spam.spam"))
	("spam.spam"
	 (spam-contents gnus-group-spam-classification-spam)
	 (spam-process ((spam spam-use-bogofilter)
			(ham spam-use-bogofilter)
			(spam spam-use-BBDB)
			(ham spam-use-BBDB)))
	 (ham-process-destination "nnfolder:mail.misc"))
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


;; (require 'gnus-group-split-fancy)
;; verify/decrypt only if mml knows about the protocol used
;; (setq mm-verify-option 'known)
;; (setq mm-decrypt-option 'known)

;; Here we make button for the multipart 
;; (setq gnus-buttonized-mime-types '("multipart/encrypted" "multipart/signed"))

;; Automcatically sign when sending mails
;; (add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

;; set up things so that groups get ranked and sorted by rank
;; (add-hook 'gnus-summary-exit-hook 'gnus-summary-bubble-group)
;; (add-hook 'gnus-summary-exit-hook 'gnus-group-sort-groups-by-rank)

;; scoring when messages sent
;; (add-hook 'message-sent-hook 'gnus-score-followup-article)
;; (add-hook 'message-sent-hook 'gnus-score-followup-thread)

;; set timestamp on groups when we select them
;; (add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

;; line, mode-line formats
;; (setq gnus-article-mode-line-format "%g [%z|%w] %S%m"
;;       gnus-group-line-format "%S%M%5y:%P %(%-30,30c%)%3O %-20,20ud %4us\n"
;;       gnus-summary-line-format "%10&user-date; %U%R%z%I%(%[%4L: %-20,20uB%]%)%O%s\n"
;;       gnus-summary-mode-line-format "%G <%A|%z> %Z [%E]"
;;       gnus-user-date-format-alist '(((gnus-seconds-today) . "%H:%M")
;; 				    (604800 . "%a %H:%M")
;; 				    ((gnus-seconds-month) . "%a %d")
;; 				    ((gnus-seconds-year) . "%b %d")
;; 				    (t . "%Y-%m-%d")))

;; ;;; util functions needed by article line format
;; (defun gnus-user-format-function-d (headers)
;;   (let ((time (gnus-group-timestamp gnus-tmp-group)))
;;     (if time
;; 	(format-time-string "%Y-%m-%d %T" time)
;; 	"")))
;; (defun gnus-user-format-function-s (headers)
;;   (let ((score (gnus-info-score (nth 2 (gnus-group-entry gnus-tmp-group)))))
;;     (if score (format "%d" score) "")))


;; (setq 
;;  gnus-article-sort-functions '(gnus-article-sort-by-number gnus-article-sort-by-score) ; sort by score and number
;;  gnus-auto-extend-newsgroup t	; automatically pull more in when requested
;;  gnus-auto-expirable-newsgroups nil ; all newsgroups are expirable
;;  gnus-auto-select-first nil	 ; do not automatically open first article
;;  gnus-check-bogus-newsgroups t	 ; always check for bogus newsgroups
;;  gnus-decay-scores t		 ; non-permanent scores decay over time
;;  gnus-default-article-saver 'gnus-summary-save-in-mail ; love me some Unix format
;;  gnus-fetch-old-headers t		; for building threads
;;  gnus-gcc-mark-as-read t		; carbon-copies should be auto-read
;;  ghnus-group-sort-function 'gnus-group-sort-by-rank ; how to sort the groups
;;  gnus-ignored-from-addresses "damned@theworld\\|verdammelt@gmail\\|mjs@theworld" ;me in many ways
;;  gnus-kill-files-directory (expand-file-name "score-files" gnus-directory) ;where to put the kill files
 
;;  gnus-message-archive-method "nnfolder" ; method for sent messages
;;  gnus-message-archive-group "nnfolder:inbox" ; were to put sent messages
 
;;  gnus-save-score t			; why would anyone want this set to nil?
;;  gnus-score-find-score-files-function '(gnus-score-find-hierarchical bbdb/gnus-score) ; how to find score file
;;  gnus-score-thread-simplify t	; subject scoring like thread names
 
;;  gnus-secondary-select-methods '((nnfolder "")) ; where to find my mails
;;  gnus-select-method '(nntp "news.gmane.org") ; where to find my news.
 
;;  gnus-subscribe-newsgroup-method 'gnus-subscribe-killed ; bring in groups killed
;;  gnus-summary-display-while-building 100		     ; show some articles while building threads
 
;;  gnus-summary-expunge-below -1000			     ; anything that sucks this much - just remove it.
 
;;  gnus-save-duplicate-list t			     ; save info about duplicates
;;  gnus-summary-ignore-duplicates t			     ; no duplicates!
;;  gnus-suppress-duplicates t                             ; automatically mark duplicates as read
;;  gnus-thread-hide-subtree t	; hide all threads initially
 
;;  gnus-thread-sort-functions 	; how to sort threads
;;  '(gnus-thread-sort-by-number (not gnus-thread-sort-by-most-recent-date) gnus-thread-sort-by-total-score)
 
;;  ;;
;;  ;; treatments
;;  ;;
;;  gnus-treat-strip-leading-blank-lines t ; get rid of any blank lines at start
;;  gnus-treat-strip-trailing-blank-lines t ; get rid of any blank lines at end
;;  gnus-treat-unsplit-urls t		      ; fix split urls
 
;;  gnus-visible-headers		; I want to see the spam score
;;  "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Organization:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:\\|^X-Spam.*:"
 
;;  mail-source-directory (concat gnus-directory "/incoming") ; where the mail is located
;;  message-directory gnus-directory	; where mail is located
 
;;  ;; delete incoming mail temp file after 1 day.
;;  mail-source-primary-source (car mail-sources) ;check for ne wmail
 
;;  ;; nnmail
;;  nnmail-crosspost nil		; if split matches multiple inboxes - crosspost
;;  nnmail-expiry-target 'nnmail-fancy-expiry-target ; fancy expiry
;;  nnmail-fancy-expiry-targets '(("from" ".*" "nnfolder:archive-%Y")) ; where to expire messages to
;;  nnmail-expiry-wait 7		       ; expire after 7 days
;;  nnmail-extra-headers '(To Newsgroups Cc) ; parse CC header as well
 
;;  nnmail-cache-accepted-message-ids t ; Gcc'd articles go in duplicate cache
 
;;  nnmail-use-long-file-names t ; use group names as filenames instead of directory trees
;;  )

;; group parameters
;; (setq gnus-parameters 
;;       '(("^nnfolder:.*"
;; 	 (gcc-self . t)
;; 	 (spam-process-destination . "nnfolder:spam")
;; 	 (ham-process-destination . "nnfolder:inbox")
;; 	 (spam-process (ham spam-use-bogofilter))
;; 	 (spam-process (spam spam-use-bogofilter)))
;; 	("list\\..*" 
;; 	 (expiry-wait . 15)
;; 	 (banner . signature)
;; 	 (total-expire . t)
;; 	 (expiry-target . delete))
;;         ("list\\.baznex"
;;          (to-address . "baznex@googlegroups.com"))
;;         ("list\\.cleancoder"
;;          (to-address . "cleancoders@yahoogroups.com"))
;; 	("list\\.nutshell"
;; 	 (to-address . "mailer@nutshellmail.com"))
;; 	("list\\.meetup"
;; 	 (split-regexp . "meetup\\.com"))
;; 	("list\\.misc"
;; 	 (extra-aliases "service@youtube.com"
;;                         "lj_notify@livejournal.com"
;;                         "eskeptic@skeptic.com"
;;                         "mini-air@air.harvard.edu"
;;                         "mitmuseum@mitmuseum.pmailus.com"
;;                         "News@insideapple.apple.com"
;;                         "ELine@cambridgema.gov"
;;                         "info@harvard.com"
;;                         "staples@e.staples.com"))
;; 	("list\\.dailylit"
;; 	 (subscribed . t )
;; 	 (extra-aliases "books@dailylit.com"))
;; 	("list\\.software-craftsmanship\\.boston"
;; 	 (subscribed . t)
;; 	 (to-address . "boston-software-craftsmanship@googlegroups.com"))
;; 	("list\\.awotd"
;; 	 (subscribed . t)
;; 	 (split-regexp . "wordsmith\\.org"))
;; 	("list\\.bank"
;; 	 (split-regexp . "citizensbankonline\\.com")
;; 	 (extra-aliases "email@transfers.ally.com"))
;; 	("list\\.fitnesse"
;; 	 (subscribed . t)
;; 	 (to-address . "fitnesse@yahoogroups.com")
;; 	 (posting-style (address "verdammelt@gmail.com")))
;; 	("list\\.bikes"
;; 	 (extra-aliases "bostonareacycling@googlegroups.com" 
;; 			"baystatecycling@googlegroups.com" 
;; 			"massbike-boston@googlegroups.com" 
;; 			"list@bostoncriticalmass.org"
;;                         "bikeinfo@massbike.org")
;; 	 (split-regexp . "livablestreets\\.info"))
;; 	("list\\.agile.\\bazaar"
;; 	 (subscribed . t)
;; 	 (to-address . "agilebazaar@yahoogroups.com")
;; 	 (extra-aliases "webmaster@agilebazaar.org"))
;; 	("list\\.posterous"
;; 	 (split-regexp . "posterous\\.com"))
;; 	("list\\.twitter"
;; 	 (extra-aliases  "*@postmaster.twitter.com"
;; 			 "notices@twimailer.com"
;;                          "sam@twittercounter.com"))
;; 	("list\\.linkedin"
;; 	 (split-regexp . "linkedin\\.com"))
;; 	("list\\.mercuryapp"
;; 	 (subscribed . t)
;; 	 (split-regexp . "mercuryapp\\.com")
;;          (posting-style (address "verdammelt@gmail.com")))
;; 	("list\\.dnd\\.forums"
;; 	 (extra-aliases "forums@dragonsfoot.org"
;; 			"admin@knights-n-knaves.com"))
;; 	("list\\.travis"
;; 	 (extra-alises "notifications@travis-ci.org"))
;; 	("tnef"
;; 	 (to-address . "verdammelt@users.sourceforge.net"))
;; 	("inbox" 
;; 	 (display . [unread])
;; 	 (total-expire . t))
;; 	("spam$" 
;; 	 (spam-contents spam)
;; 	 (total-expire . t)
;; 	 (expiry-target . delete))
;; 	("archive\\.*" 
;; 	 (gnus-use-scoring nil) 
;; 	 (gnus-use-adaptive-scoring nil)

;; 	 (spam-process-destination . "nnfolder:spam")
;; 	 (ham-process-destination . "nnfolder:inbox")
;; 	 (spam-process (ham spam-use-bogofilter))
;; 	 (spam-process (spam spam-use-bogofilter))
;; )
;; 	("^gmane\\."
;; 	 (spam-autodetect . t)
;; 	 (spam-autodetect-methods spam-use-bogofilter 
;; 				  spam-use-regex-headers)
;; 	 (spam-process (spam spam-use-gmane)))))


;; ;; splitting:
;; ;; 1. split to where the parent went
;; ;; 2. split by group parameters
;; ;; 3. split by spam
;; ;; 4. dump into inbox
;; (setq  nnmail-split-methods 'nnmail-split-fancy
;;        nnmail-split-fancy '(| ;(: gnus-registry-split-fancy-with-parent)
;; 			      (: gnus-group-split-fancy)
;; 			      (: spam-split)
;; 			      "inbox")
;;        gnus-group-split-default-catch-all-group "inbox")

;; ;; spam setup
;; (setq spam-log-to-registry t		; to keep from multiply handling a piece of mail
;;       ;; spam-use-BBDB t			; TODO: this is the problem!
;;       spam-use-bogofilter t
;;       spam-use-regex-headers t
;;       spam-use-spamassassin-headers t
      
;;       gnus-spam-newsgroup-contents 	; anything in spam is spam, anything in inbox is ham
;;       '(("spam" gnus-group-spam-classification-spam)
;; 	("inbox" gnus-group-spam-classification-ham)))

;; (spam-initialize)			; initialize spam processing

;; makeing sure we can open urls in external browsers (f1 if hitting return didn't work)
;; (eval-after-load "w3m"
;;  '(progn 
;;     (add-to-list 'w3m-minor-mode-command-alist '(w3m-view-url-with-external-browser))
;;     (define-key gnus-article-mode-map (kbd "<f1>") 'w3m-view-url-with-external-browser)))
