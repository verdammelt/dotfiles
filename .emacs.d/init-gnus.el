;;;;
;;;; GNUS
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Time-stamp: <2014-01-22 20:10:55 mark>
;;;;
;;;
;;; TODO: 
;;; * sent mail goes to inbox.
;;; * more splitting
;;;

;;; 
;;; Select methods
;;;
(after 'gnus
  (require 'bbdb)

  (setq gnus-init-file (locate-user-emacs-file "init-gnus.el"))

  (setq 
   gnus-select-method '(nntp "news.gmane.org") ; where to find my news.
   gnus-secondary-select-methods '((nnfolder ""))) ; where to find my mails

  (setq
   gnus-treat-display-x-face (quote head)
   gnus-treat-display-face 'head
   gnus-treat-from-gravatar (quote head)
   gnus-treat-from-picon (quote head)
   gnus-treat-mail-gravatar (quote head)
   gnus-treat-mail-picon (quote head)
   gnus-treat-newsgroups-picon (quote head)
   gnus-treat-display-smileys t
   gnus-treat-unsplit-urls t
   gnus-treat-strip-multiple-blank-lines t
   gnus-treat-emphasis t
   gnus-treat-x-pgp-sig t
   )
;; 
;;; General settings
;;;
  (setq 
   gnus-decay-scores t	  ; temporary scores should degrade over time.
   gnus-kill-files-directory (expand-file-name "score-files" gnus-directory) ;where to put the kill files
   gnus-gcc-mark-as-read t	   ; carbon-copies should be auto-read
   
   ;; formatting the screen
   gnus-summary-line-format 
   "[%4i]%U%R %10&user-date;%[%4L: %-23,23uB%]%O%B%s\n"
   
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
				(not gnus-thread-sort-by-most-recent-date)
				gnus-thread-sort-by-total-score)
   gnus-sort-gathered-threads-function 'gnus-thread-sort-by-date
   gnus-thread-hide-subtree t
   gnus-summary-make-false-root 'dummy
   gnus-summary-gather-subject-limit 'fuzzy
   gnus-build-sparse-threads 'some
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
   spam-use-bogofilter t ; I want to fine tune the spam checking with local bogofilter
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
			(to "codeandcocktails@gmail.com" "mail.codeandcocktails")
			(to "noreply@sourceforge.net" "mail.tnef")
			(any ".*2u.com" "cyrus.2u")
			(| (to "msimpson@cyrusinnovation.com" "cyrus.inbox")
			   (any ".*cyrusinnovation.com" "cyrus.inbox")
			   (any ".*cyruslists.com" "cyrus.inbox"))
			(to "discuss-bawch@googlegroups.com" "list.bawch")
			(to "clean-code-discussion@googlegroups.com" "list.cleancode")
			(from "wsmith@wordsmith.org" "list.awotd")
			(| (any ".*ally.*" "list.bank")
			   (any ".*mint.*" "list.bank")
			   (any ".*citizensbank.*" "list.bank"))
			(to "boston-software-crafstmanship@googlegroups.com" "list.boston-software-crafstmanship")
			(from "books@dailylit.com" "list.dailylit")
			(| (any "ELine@cambridgema.gov" "list.misc")
			   (any "info@harvard.com" "list.misc")
			   (any ".*zipcarmail.com" "list.misc"))
			(| (any ".*flickr" "list.social-media")
			   (any ".*facebookmail" "list.social-media")
			   (any ".*twitter" "list.social-media")
			   (any ".*linkedin" "list.social-media")
			   (any ".*@exercism.io" "list.social-media"))

			(: gnus-registry-split-fancy-with-parent)
			(: gnus-group-split-fancy nil t nil)
			(: spam-split)

			"mail.inbox")
   )
  ;; patched version to reads from gnus-parameters correctly
  (load (locate-user-emacs-file "lisp/gnus-group-split-fancy"))
  
;;;
;;; Scoring
;;;
  (after 'gnus-score
    (add-hook 'message-sent-hook 'gnus-score-followup-article)
    (add-hook 'message-sent-hook 'gnus-score-followup-thread)
    (setq 
     gnus-use-adaptive-scoring t
     gnus-score-find-score-files-function 
     '((lambda (group) '("SCORE")) 
       gnus-score-find-hierarchical)
     gnus-adaptive-pretty-print t
     gnus-adaptive-word-no-group-words t
     )
    (add-to-list 'gnus-default-adaptive-score-alist
		 '(gnus-ticked-mark (subject 10))))
  
;;;
;;; Expiry
;;;
;;; expire mail to an archive mailspool for the year.
;;;
  (setq 
   gnus-message-archive-group 
   '((format-time-string "archive-%Y"))
   
   nnmail-expiry-target 'nnmail-fancy-expiry-target
   nnmail-expiry-wait 28		; originally 7
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
	   (posting-style  (address "codeandcocktails@gmail.com")))

	  ("mail.tnef"
	   (total-expire . nil)
	   (auto-expire . nil))

	  ("list.*"
	   (total-expire . t)
	   (expiry-target . delete))

	  ("cyrus.*"
	   (gcc-self .t)
	   (total-expire . nil)
	   (posting-style (address "msimpson@cyrusinnovation.com")))

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
;;; utility items
;;;
  (add-to-list 'auto-mode-alist '("SCORE$" . lisp-mode))
  (add-to-list 'auto-mode-alist '("ADAPT$" . lisp-mode)))


;; keybinding for gnus
(defun switch-to-gnus () 
  (interactive) 
  (let ((group-buffer (get-buffer "*Group*")))
    (if group-buffer (switch-to-buffer group-buffer)
      (gnus))))

(global-set-key (kbd "<f6>") 'switch-to-gnus)

(setq read-mail-command 'gnus)

(provide 'init-gnus)
