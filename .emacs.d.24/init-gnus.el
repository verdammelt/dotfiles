;;;
;;;; .gnus.el
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Time-stamp: <2011-11-05 18:12:22 mark>
;;;;
(require 'gnus-group-split-fancy)

(add-to-list 'auto-mode-alist '("SCORE$" . lisp-mode))
(add-to-list 'auto-mode-alist '("ADAPT$" . lisp-mode))

;; verify/decrypt only if mml knows about the protocl used
(setq mm-verify-option 'known)
(setq mm-decrypt-option 'known)

;; Here we make button for the multipart 
(setq gnus-buttonized-mime-types '("multipart/encrypted" "multipart/signed"))

;; Automcatically sign when sending mails
(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

;; set up things so that groups get ranked and sorted by rank
(add-hook 'gnus-summary-exit-hook 'gnus-summary-bubble-group)
(add-hook 'gnus-summary-exit-hook 'gnus-group-sort-groups-by-rank)

;; scoring went messages sent
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)

;; set timestamp on groups when we select them
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)

;; line, mode-line formats
(setq gnus-article-mode-line-format "%g [%z|%w] %S%m"
      gnus-group-line-format "%S%M%5y:%P %(%-30,30c%)%3O %-20,20ud %4us\n"
      gnus-summary-line-format "%10&user-date; %U%R%z%I%(%[%4L: %-20,20uB%]%)%O%s\n"
      gnus-summary-mode-line-format "%G <%A|%z> %Z [%E]"
      gnus-user-date-format-alist '(((gnus-seconds-today) . "%H:%M")
				    (604800 . "%a %H:%M")
				    ((gnus-seconds-month) . "%a %d")
				    ((gnus-seconds-year) . "%b %d")
				    (t . "%Y-%m-%d")))

;;; util functions needed by article line format
(defun gnus-user-format-function-d (headers)
  (let ((time (gnus-group-timestamp gnus-tmp-group)))
    (if time
	(format-time-string "%Y-%m-%d %T" time)
	"")))
(defun gnus-user-format-function-s (headers)
  (let ((score (gnus-info-score (nth 2 (gnus-group-entry gnus-tmp-group)))))
    (if score (format "%d" score) "")))


(setq gnus-article-sort-functions 
      '(gnus-article-sort-by-number gnus-article-sort-by-score) ; sort by score and number
      gnus-agent-directory (expand-file-name ".agent" gnus-directory) ; TODO: revert to default?
      gnus-cache-directory (expand-file-name ".cache" gnus-directory) ; TODO: revert to default?
      gnus-duplicate-file (expand-file-name ".suppression" gnus-directory) ;TODO: revert to default?
      gnus-auto-extend-newsgroup t	; automatically pull more in when requested
      gnus-auto-expirable-newsgroups nil ; all newsgroups are expirable
      gnus-auto-select-first nil	 ; do not automatically open first article
      gnus-check-bogus-newsgroups t	 ; always check for bogus newsgroups
      gnus-decay-scores t		 ; non-permanent scores decay over time
      gnus-default-article-saver 'gnus-summary-save-in-mail ; love me some Unix format
      gnus-fetch-old-headers t		; for building threads
      gnus-gcc-mark-as-read t		; carbon-copies should be auto-read
      ghnus-group-sort-function 'gnus-group-sort-by-rank ; how to sort the groups
      gnus-ignored-from-addresses "damned@theworld\\|verdammelt@gmail\\|mjs@theworld" ;me in many ways
      gnus-kill-files-directory (expand-file-name "score-files" gnus-directory) ;where to put the kill files

      gnus-message-archive-method "nnfolder" ; method for sent messages
      gnus-message-archive-group "nnfolder:inbox" ; were to put sent messages

      gnus-save-score t			; why would anyone want this set to nil?
      gnus-score-find-score-files-function '(gnus-score-find-hierarchical bbdb/gnus-score) ; how to find score file
      gnus-score-thread-simplify t	; subject scoring like thread names

      gnus-secondary-select-methods '((nnfolder "") (nntp "nntp.theworld.com")) ; where to find my mails
      gnus-select-method '(nntp "news.gmane.org") ; where to find my mails.

      gnus-subscribe-newsgroup-method 'gnus-subscribe-killed ; bring in groups killed
      gnus-summary-display-while-building 100		     ; show some articles while building threads

      gnus-summary-expunge-below -1000			     ; anything that sucks this much - just remove it.

      gnus-save-duplicate-list t			     ; save info about duplicates
      gnus-summary-ignore-duplicates t			     ; no duplicates!
      gnus-suppress-duplicates t                             ; automatically mark duplicates as read
      gnus-thread-hide-subtree t	; hide all threads initially
      gnus-thread-indent-level 2	; how much to indent each thread TODO: revert to default? (4)

      gnus-thread-sort-functions 	; how to sort threads
      '(gnus-thread-sort-by-number (not gnus-thread-sort-by-most-recent-date) gnus-thread-sort-by-total-score)

      ;;
      ;; treatments
      ;;
      gnus-treat-body-boundary 'head	; TODO: revert to default
      gnus-treat-strip-leading-blank-lines t ; get rid of any blank lines at start
      gnus-treat-strip-trailing-blank-lines t ; get rid of any blank lines at end
      gnus-treat-unsplit-urls t		      ; fix split urls

      gnus-visible-headers		; I want to see the spam score
      "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Organization:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:\\|^X-Spam.*:"

      mail-source-directory gnus-directory ; where the mail is located
      message-directory gnus-directory	; where mail is located

      ;; delete incoming mail temp file after 1 day.
      mail-source-primary-source (car mail-sources) ;check for ne wmail
      mail-source-delete-incoming 1		    ; delete incoming files after 1 day

      ;; setup message rendering with w3m and inlin images.
      mm-text-html-renderer 'w3m	; TODO: revert to default
      mm-inline-large-images t		; show any size of images  TODO: revert to default
      mm-inline-text-html-with-images t	; show images in inline html TODO: revert to default
      mm-inline-text-html-with-w3m-keymap nil ; don't like the w3m keymaps
      w3m-default-display-inline-images t     ; show inline images for w3m TODO: revert to default

      ;; nnfolder TODO: revert to default on all these
      nnfolder-active-file (expand-file-name ".active" gnus-directory) ; "/home/damned/Mail/.active" 
      nnfolder-newsgroups-file (expand-file-name ".newsgroups" gnus-directory) ; "/home/damned/Mail/.newsgroups"
      nnfolder-marks-directory (expand-file-name ".marks" gnus-directory) ; "/home/damned/Mail/.marks"
      nnfolder-nov-directory (expand-file-name ".nov" gnus-directory) ; "/home/damned/Mail/.nov"

      ;; nnmail
      nnmail-crosspost nil		; if split matches multiple inboxes - crosspost
      nnmail-expiry-target 'nnmail-fancy-expiry-target ; fancy expiry
      nnmail-fancy-expiry-targets '(("from" ".*" "nnfolder:archive-%Y")) ; where to expire messages to
      nnmail-expiry-wait 7		; expire after 7 days
      nnmail-extra-headers '(To Newsgroups Cc) ; parse CC header as well

      nnmail-cache-accepted-message-ids t ; Gcc'd articles go in duplicate cache

      nnmail-use-long-file-names t	; use group names as filenames instead of directory trees


      ;; TODO: revert to default
      nntp-marks-directory (expand-file-name ".marks" gnus-directory) ; where to nntp marks
      )

;; group parameters
(setq gnus-parameters 
      '(("^nnfolder:.*"
	 (gcc-self . t))
	("list\\..*" 
	 (expiry-wait . 15)
	 (banner . signature)
	 (total-expire . t)
	 (expiry-target . delete))
        ("list\\.baznex"
         (to-address . "baznex@googlegroups.com"))
        ("list\\.cleancoder"
         (to-address . "cleancoders@yahoogroups.com"))
	("list\\.nutshell"
	 (to-address . "mailer@nutshellmail.com"))
	("list\\.meetup"
	 (split-regexp . "meetup\\.com"))
	("list\\.misc"
	 (extra-aliases "service@youtube.com"
                        "lj_notify@livejournal.com"
                        "eskeptic@skeptic.com"
                        "mini-air@air.harvard.edu"
                        "mitmuseum@mitmuseum.pmailus.com"
                        "News@insideapple.apple.com"
                        "ELine@cambridgema.gov"
                        "info@harvard.com"
                        "staples@e.staples.com"))
	("list\\.dailylit"
	 (subscribed . t )
	 (extra-aliases "books@dailylit.com"))
	("list\\.software-craftsmanship\\.boston"
	 (subscribed . t)
	 (to-address . "boston-software-craftsmanship@googlegroups.com"))
	("list\\.awotd"
	 (subscribed . t)
	 (split-regexp . "wordsmith\\.org"))
	("list\\.bank"
	 (split-regexp . "citizensbankonline\\.com")
	 (extra-aliases "email@transfers.ally.com"))
	("list\\.fitnesse"
	 (subscribed . t)
	 (to-address . "fitnesse@yahoogroups.com")
	 (posting-style
	  (address "verdammelt@gmail.com")))
	("list\\.bikes"
	 (extra-aliases "bostonareacycling@googlegroups.com" 
			"baystatecycling@googlegroups.com" 
			"massbike-boston@googlegroups.com" 
			"list@bostoncriticalmass.org"
                        "bikeinfo@massbike.org")
	 (split-regexp . "livablestreets\\.info"))
	("list\\.agile.\\bazaar"
	 (subscribed . t)
	 (to-address . "agilebazaar@yahoogroups.com")
	 (extra-aliases "webmaster@agilebazaar.org"))
	("list\\.posterous"
	 (split-regexp . "posterous\\.com"))
	("list\\.twitter"
	 (extra-aliases  "notices@twimailer.com"
                         "sam@twittercounter.com"))
	("list\\.linkedin"
	 (split-regexp . "linkedin\\.com"))
	("list\\.mercuryapp"
	 (subscribed . t)
	 (split-regexp . "mercuryapp\\.com")
         (posting-style
	  (address "verdammelt@gmail.com")))
	("list\\.dnd\\.forums"
	 (extra-aliases "forums@dragonsfoot.org"
			"admin@knights-n-knaves.com"))
	("tnef"
	 (to-address . "verdammelt@users.sourceforge.net"))
	("inbox" 
	 (display . [unread])
	 (total-expire . t)
	 (spam-process (ham spam-use-bogofilter)))
	("spam$" 
	 (spam-process (spam spam-use-bogofilter))
	 (total-expire . t)
	 (expiry-target . delete))
	("archive\\.*" 
	 (gnus-use-scoring nil) 
	 (gnus-use-adaptive-scoring nil) )
	("^gmane\\."
	 (spam-process (spam spam-use-gmane)))))

(setq 
 spam-log-to-registry t
 ;; spam-use-BBDB t			; this is the problem!
 spam-use-bogofilter t
 spam-use-regex-headers t
 spam-use-spamassassin-headers t

 gnus-spam-newsgroup-contents 	; anything in spam is spam, anything in inbox is ham
 '(("spam" gnus-group-spam-classification-spam)
   ("inbox" gnus-group-spam-classification-ham))
 gnus-spam-process-destinations '(("inbox" "nnfolder:spam")) ; if in inbox and marked spam - send to spam
 gnus-ham-process-destinations '(("spam" "nnfolder:inbox"))  ; if in spam and marked ham - send to inbox

 gnus-group-split-default-catch-all-group "inbox" 

 ;; splitting:
 ;; 1. split to where the parent went
 ;; 2. split by group parameters
 ;; 3. split by spam
 ;; 4. dump into inbox
 nnmail-split-methods 'nnmail-split-fancy
 nnmail-split-fancy '(| (: gnus-registry-split-fancy-with-parent)
			(: gnus-group-split-fancy)
			(: spam-split)
			"inbox"))

(spam-initialize)			; initialize spam processing




;; makeing sure we can open urls in external browsers (f1 if hitting return didn't work)
(eval-after-load "w3m"
 '(progn 
    (add-to-list 'w3m-minor-mode-command-alist '(w3m-view-url-with-external-browser))
    (define-key gnus-article-mode-map (kbd "<f1>") 'w3m-view-url-with-external-browser)))


;; DEMONS!
(gnus-demon-add-scan-timestamps)	; setting timestamps
(gnus-demon-add-scanmail)		; get the new mails
(gnus-demon-init)			; poke the daemon to get it going

(setq gnus-registry-install t		; yes we use the registry
      gnus-registry-split-strategy 'majority ; splitting to the place that gets the most 'votes'
      gnus-registry-ignored-groups '(("nntp" t) ;don't do registry for these
                                     ("spam" t))
      gnus-registry-max-entries 50000)
(gnus-registry-initialize)		; fire up the registry

(gnus-compile)		  ; doc claims that this will speed things up.

