;;;
;;; BBDB
;;;
;;; Modified Time-stamp: <2011-12-09 23:02:25 mark>
;;;
(require 'bbdb-autoloads)
(autoload 'bbdb-expire-bbdb "bbdb-expire" "Remove old items from BBDB" t)

(eval-after-load 'bbdb 
  '(progn 
     (setup-bbdb-expire)
     
     (setq bbdb-complete-name-allow-cycling t
	   bbd/mail-auto-create-p nil ; because we use bbdb for spam white listing.
	   bbd/news-auto-create-p nil ; because we use bbdb for spam white listing.
	   bbdb-quiet-about-name-mismatches 30) ; show name mismatches for a short time
     
     (eval-after-load 'supercite '(bbdb-insinuate-sc))
     (eval-after-load 'gnus '(bbdb-initialize 'gnus))
     (eval-after-load 'message '(bbdb-initialize 'message))))

(defun setup-bbdb-expire ()
  (eval-after-load 'bbdb-expire
    '(progn
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

       (bbdb-expire-initialize)))

  (add-hook 'midnight-hook #'bbdb-expire-bbdb))

(defun bbdb-message-mode-keys ()
  (define-key message-mode-map (kbd "M-TAB") 'bbdb-complete-name))

(provide 'init-bbdb)
