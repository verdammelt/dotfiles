;;;;
;;;; .gnus.el
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Time-stamp: <2011-11-05 18:12:22 mark>
;;;;
;; (bbdb-initialize 'gnus)
;; (bbdb-initialize 'message)
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

;; Enough explicit settings
(setq pgg-scheme 'epg)
(setq epg-gpg-program "gpg2")

;;; set up the pgp stuff.
(eval-after-load "mm-decode"
  '(progn 
     (add-to-list 'mm-inlined-types "application/pgp$")
     (add-to-list 'mm-inline-media-tests '("application/pgp$"
					   mm-inline-text identity))
     (add-to-list 'mm-automatic-display "application/pgp$")
     (setq mm-automatic-display (remove "application/pgp-signature"
					mm-automatic-display)
	   pgg-query-keyserver t)))

;;;
;;; HOOKS
;;;
(add-hook 'gnus-summary-exit-hook 'gnus-summary-bubble-group)
(add-hook 'gnus-summary-exit-hook 'gnus-group-sort-groups-by-rank)
(add-hook 'message-sent-hook 'gnus-score-followup-article)
(add-hook 'message-sent-hook 'gnus-score-followup-thread)
(add-hook 'gnus-select-group-hook 'gnus-group-set-timestamp)
;; tweak for the mode-line to fit it all in there.
(add-hook 'display-time-hook
	  (lambda () (setq gnus-mode-non-string-length
			   (+ 21
			      (if line-number-mode 5 0)
			      (if column-number-mode 4 0)
			      (length display-time-string)))))

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

(setq
      gnus-article-sort-functions '(gnus-article-sort-by-number gnus-article-sort-by-score)
      gnus-agent-directory (expand-file-name ".agent" gnus-directory)
      gnus-cache-directory (expand-file-name ".cache" gnus-directory) 
      gnus-duplicate-file (expand-file-name ".suppression" gnus-directory)
      gnus-auto-extend-newsgroup t
      gnus-auto-expirable-newsgroups nil
      gnus-auto-select-first nil
      gnus-check-bogus-newsgroups t
      gnus-conirm-mail-reply-to-news t
      gnus-decay-scores t
      gnus-default-article-saver 'gnus-summary-save-in-mail
      gnus-default-adaptive-score-alist
      '((gnus-kill-file-mark)
	(gnus-unread-mark)
	(gnus-ticked-mark (from 3) (subject 30))
	(gnus-dormant-mark (from 2) (subject 20))
	(gnus-expirable-mark)
	(gnus-ancient-mark)
	(gnus-low-score-mark)
	(gnus-read-mark (from 3) (subject 30))
	(gnus-catchup-mark (subject -10)) 
	(gnus-killed-mark (from -1) (subject -20))
	(gnus-del-mark (from -2) (subject -15)))
      gnus-fetch-old-headers 'some
      gnus-gcc-mark-as-read t
      gnus-group-sort-function 'gnus-group-sort-by-rank
      gnus-ignored-from-addresses "damned@theworld\\|verdammelt@gmail\\|mjs@theworld"
      gnus-kill-files-directory (expand-file-name "score-files" gnus-directory)

      gnus-message-archive-method "nnfolder"
      gnus-message-archive-group "nnfolder:inbox"
;      gnus-permanently-visible-groups "nnfolder:inbox"

      gnus-play-startup-jingle t

      gnus-save-score t
      gnus-score-find-score-files-function '(gnus-score-find-hierarchical bbdb/gnus-score)
      gnus-score-thread-simplify t
      gnus-secondary-select-methods '((nnfolder "") (nntp "nntp.theworld.com"))
	
      gnus-select-method '(nntp "news.gmane.org")
      gnus-subscribe-newsgroup-method 'gnus-subscribe-killed
      gnus-summary-display-while-building 100
      gnus-summary-expunge-below -1000
      gnus-summary-ignore-duplicates t

      gnus-suppress-duplicates t
      gnus-save-duplicate-list t
      gnus-thread-hide-subtree t
      gnus-thread-indent-level 2
      gnus-thread-sort-functions '(gnus-thread-sort-by-number
				   (not gnus-thread-sort-by-most-recent-date)
				   gnus-thread-sort-by-total-score
				   )
      gnus-treat-highlight-headers 'head
      gnus-treat-body-boundary 'head
      gnus-treat-overstrike t
      gnus-treat-fill-article nil
      gnus-treat-fill-long-lines nil
      gnus-treat-strip-cr nil
      gnus-treat-strip-leading-blank-lines t
      gnus-treat-strip-trailing-blank-lines t
      gnus-treat-unsplit-urls t
      gnus-topic-display-empty-topics nil
      gnus-use-adaptive-scoring '(line)
      gnus-visible-headers "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Organization:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:\\|^X-Spam.*:"

      ;; delete incoming mail temp file after 1 day.
      mail-sources '((file))
      mail-source-directory gnus-directory
      mail-source-primary-source (car mail-sources)
      mail-source-delete-incoming 1
      mail-source-delete-old-incoming-confirm nil

      message-directory gnus-directory			     
      message-auto-save-directory (expand-file-name "drafts" gnus-directory)

      ;; setup message rendering with w3m and inlin images.
      mm-text-html-renderer 'w3m
      mm-inline-large-images t
      mm-inline-text-html-with-images t
      mm-inline-text-html-with-w3m-keymap nil
      w3m-default-display-inline-images t

      ;; nnmbox -- currently unused.
      nnmbox-mbox-file (expand-file-name "mbox" gnus-directory) ; "/home/damned/Mail/mbox"
      nnmbox-active-file (expand-file-name ".mbox-active" gnus-directory) ; "/home/damned/Mail/.mbox-active" 

      ;; nnfolder
      nnfolder-active-file (expand-file-name ".active" gnus-directory) ; "/home/damned/Mail/.active"
      nnfolder-newsgroups-file (expand-file-name ".newsgroups" gnus-directory) ; "/home/damned/Mail/.newsgroups"
      nnfolder-marks-directory (expand-file-name ".marks" gnus-directory) ; "/home/damned/Mail/.marks"
      nnfolder-nov-directory (expand-file-name ".nov" gnus-directory) ; "/home/damned/Mail/.nov"

      ;; nnmail
      nnmail-crosspost nil
      nnmail-expiry-target 'nnmail-fancy-expiry-target
      nnmail-fancy-expiry-targets '(("from" ".*" "nnfolder:archive-%Y"))
      nnmail-expiry-wait 7
      nnmail-extra-headers '(To Newsgroups Cc)

      nnmail-cache-accepted-message-ids t


      nnmail-use-long-file-names t

      nnrss-directory (expand-file-name ".rss" gnus-directory)

      nntp-marks-directory (expand-file-name ".marks" gnus-directory)
      )

;; mail splitting
(setq nnmail-split-methods 'nnmail-split-fancy
      nnmail-split-fancy
      '(| (: gnus-registry-split-fancy-with-parent)
	  (: gnus-group-split-fancy)
	  (: spam-split)
	  "inbox")
      gnus-group-split-default-catch-all-group "inbox" )

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
	 (spam-process '((ham spam-use-BBDB)))
	 (display . [or unread recent])
	 (total-expire . t))
	("spam$" 
	 (total-expire . t)
	 (expiry-target . delete))
	("archive\\.*" 
	 (gnus-use-scoring nil) 
	 (gnus-use-adaptive-scoring nil) )
	("^gmane\\."
	 (spam-process (gnus-group-spam-exit-processor-report-gmane)))))

;; spam setup
(setq spam-log-to-registry t
      spam-use-BBDB t
      spam-use-BBDB-exclusive t
      spam-use-regex-headers t
      gnus-spam-newsgroup-contents '(("spam" gnus-group-spam-classification-spam)
				     ("inbox" gnus-group-spam-classification-ham))
      gnus-spam-process-destinations '(("inbox" "nnfolder:spam"))
      gnus-ham-process-destinations '(("spam" "nnfolder:inbox"))
      spam-move-spam-nonspam-grops-only nil
      gnus-mark-only-unseen-as-spam t
      spam-mark-ham-unread-before-move-from-spam-group t
      )


;; ;;; util function needed by article line format
(defun gnus-user-format-function-d (headers)
  (let ((time (gnus-group-timestamp gnus-tmp-group)))
    (if time
	(format-time-string "%Y-%m-%d %T" time)
	"")))
(defun gnus-user-format-function-s (headers)
  (let ((score (gnus-info-score (nth 2 (gnus-group-entry gnus-tmp-group)))))
    (if score (format "%d" score) "")))

(eval-after-load "w3m"
 '(progn 
    (add-to-list 'w3m-minor-mode-command-alist '(w3m-view-url-with-external-browser))
    (define-key gnus-article-mode-map (kbd "<f1>") 'w3m-view-url-with-external-browser)))


;; DEMONS!
(gnus-demon-add-scan-timestamps)
(gnus-demon-add-scanmail)
(gnus-demon-init)

(setq gnus-registry-install t)
(gnus-registry-initialize)
(spam-initialize)

;; doc claims that this will speed things up.
(gnus-compile)
