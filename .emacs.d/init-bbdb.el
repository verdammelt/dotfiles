;;;;
;;;; BBDB
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Modified Time-stamp: <2013-05-21 21:20:01 mark>
;;;;
;;;
;;; TODO
;;; * v3 upgrade
;;; * safer sync with google (merge in google contacts)
;;; * two way sync?
;;;
(require 'bbdb-autoloads)
(autoload 'bbdb-expire-bbdb "bbdb-expire" "Remove old items from BBDB" t)

(after 'bbdb 
  (setup-bbdb-expire)
  
  (setq bbdb-complete-name-allow-cycling t
	bbdb/mail-auto-create-p nil ; because we use bbdb for spam white listing.
	bbdb/news-auto-create-p nil ; because we use bbdb for spam white listing.
	bbdb-quiet-about-name-mismatches 30) ; show name mismatches for a short time
  
  (after 'supercite '(bbdb-insinuate-sc))
  (after 'gnus '(bbdb-initialize 'gnus))
  (after 'message '(bbdb-initialize 'message)))

(defun setup-bbdb-expire ()
  (after 'bbdb-expire
    (setq bbdb-expire-this-old 30)	; expiry threshold
       
    (bbdb-expire-field-foo-p 'mail-alias)
    (add-to-list 'bbdb-expire-preservation-functions
		 #'bbdb-expire-field-mail-alias-p)
    (add-hook 'bbdb-expire-pre-expire-hook
	      #'(lambda () (message "bbdb expiry start: %d records" 
				    (length (bbdb-records)))))
    (add-hook 'bbdb-expire-post-expire-hook
	      #'(lambda () (message "bbdb-expiry finish: %d records" 
				    (length (bbdb-records)))))

    (bbdb-expire-initialize)
    (add-hook 'midnight-hook #'bbdb-expire-bbdb)))

(provide 'init-bbdb)
