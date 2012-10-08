;;;
;;;; GNUS
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Time-stamp: <2012-10-07 15:13:49 mark>
;;;;
;;;
;;; TODO: 
;;; * sent mail goes to inbox.
;;; * more splitting
;;;

;;; load special version of spam.el to get fix for bbdb bug
(load "~/SRC/gnus/lisp/spam.el")

;;; 
;;; Select methods
;;;
(setq 
 gnus-select-method '(nntp "news.gmane.org") ; where to find my news.
 gnus-secondary-select-methods '((nnfolder ""))) ; where to find my mails

;;; 
;;; General settings
;;;
(setq 
 gnus-decay-scores t	  ; temporary scores should degrade over time.
 gnus-kill-files-directory (expand-file-name "score-files" gnus-directory) ;where to put the kill files
 gnus-gcc-mark-as-read t	   ; carbon-copies should be auto-read

 ;; formatting the screen
 gnus-summary-line-format 
 "[%4iA/%4VT] %10&user-date; %U%R%z%I%(%[%4L: %-20,20uB%]%)%O%s\n"

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
;;; Sorting Articles in Summary Buffer
;;;
(setq
 gnus-thread-sort-functions '(gnus-thread-sort-by-number 
			      (not gnus-thread-sort-by-most-recent-date)
			      gnus-thread-sort-by-total-score)
 gnus-thread-hide-subtree t
 gnus-thread-expunge-below -1000
 )

;;; 
;;; Registry
;;;
(setq gnus-registry-install t
      gnus-registry-split-strategy 'majority)
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
 
 nnmail-treat-duplicates 'delete
 )

;;;
;;; Spam
;;;
(setq
 spam-use-spamassassin-headers t    ; because my ISP runs spamassassin
 spam-use-bogofilter t		    ; I want to fine tune the spam checking with local bogofilter
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
 nnmail-split-fancy '(| (: gnus-group-split-fancy)
			(: spam-split)
			"mail.inbox")
 spam-split-group "spam.spam"
 )
;; patched version to reads from gnus-parameters correctly
(load (locate-user-emacs-file "lisp/gnus-group-split-fancy"))

;;;
;;; Scoring
;;;
(require 'gnus-score)
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)
(setq 
 gnus-use-adaptive-scoring t
 gnus-score-find-score-files-function '(gnus-score-find-hierarchical)
 gnus-adaptive-pretty-print t
 gnus-adaptive-word-no-group-words t
 )
(add-to-list 'gnus-default-adaptive-score-alist
	     '(gnus-ticked-mark (subject 10)))

;;;
;;; Expiry
;;;
;;; expire mail to an archive mailspool for the year.
;;;
(setq 
 gnus-message-archive-group 
 '((concat "nnfolder+archive:" (format-time-string "archive-%Y")))

 nnmail-expiry-target 'nnmail-fancy-expiry-target
 nnmail-expiry-wait 28
 nnmail-fancy-expiry-targets '(("from" ".*" "nnfolder+archive:archive-%Y")))

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
	 (gcc-self . t)
	 (total-expire . t))

	("mail.codeandcocktails"
	 (to-address . "codeandcocktails@gmail.com")
	 (posting-style . "codeandcocktails@gmail.com"))

	("mail.tnef"
	 (to-address . "noreply@sourceforge.net")
	 (total-expire . nil)
	 (auto-expire . nil))

	("list.*"
	 (total-expire . t)
	 (expiry-target . delete))

	("list\.agile-new-england"
	 (extra-aliases "webmaster@agilenewengland.org"))

	("list\.awotd"
	 (extra-aliases "wsmith@wordsmith.org"))

	("list\.bank"
	 (split-regexp . "schwab\\|ally")
	 (extra-aliases "citizensbank_customerservice@customercenter.net"))

	("list\.baznex"
	 (to-address . "baznex@googlegroups.com"))

	("list\.bikes"
	 (extra-aliases 
	  "events@massbike.org"
	  "bostonbikes@cityofboston.gov"
	  "kara@livablestreets.info"
	  "info@bostoncyclistsunion.org"
	  "bikeinfo@massbike.org"
	  "charlie@livablestreets.info"
	  "baystatecycling@googlegroups.com"
	  "BostonAreaCycling@googlegroups.com"))
	("list\.boston-software-craftsmanship"
	 (to-address . "boston-software-craftsmanship@googlegroups.com"))
	("list\.dailylit"
	 (extra-aliases "books@dailylit.com"))
	("list\.mercuryapp"
	 (extra-aliases "feelings-unicorn@mercuryapp.com"))
	("list\.misc"
	 (extra-aliases 
	  "ELine@cambridgema.gov"
	  "contact@flatearththeatre.com"
	  "info@harvard.com"
	  "underthehood@members.zipcarmail.com"
	  ))
	("list\.social-media"
	 (split-regexp . "flickr\\|facebookmail\\|twitter\\|linkedin")
	 (extra-aliases 
	  "action@ifttt.com"
	  "info@meetup.com"
	  "no-reply@posterous.com"
	  "ops@geekli.st"
	  ))

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

;; keybinding for gnus
(defun switch-to-gnus () 
  (interactive) 
  (let ((group-buffer (get-buffer "*Group*")))
    (if group-buffer (switch-to-buffer group-buffer)
	(gnus))))

(global-set-key (kbd "<f6>") 'switch-to-gnus)

(provide 'init-gnus)
