;;;
;;; BBDB setups
;;;
;;; Time-stamp: <2011-04-23 21:44:10 mark>

(require 'bbdb)
(require 'bbdb-expire)

(setq bbdb-complete-name-allow-cycling t
      bbdbhv/news-auto-create-p t
      bbdb-expire-this-old-30)

(bbdb-expire-field-foo-p 'mail-alias)
(add-to-list 'bbdb-expire-preservation-functions
	     #'bbdb-expire-field-mail-alias-p)
(add-hook 'bbdb-expire-pre-expire-hook
	  #'(lambda () (message "bbdb expiry start: %d records"
				(length (bbdb-records)))))
(add-hook 'bbdb-expire-post-expire-hook
	  #'(lambda () (message "bbdb-expiry finish: %d records"
				(length (bbdb-records)))))

(defun bbdb-message-mode-keys ()
  (define-key message-mode-map (kbd "M-TAB") 'bbdb-complete-name))

(bbdb-insinuate-sc)
(bbdb-insinuate-w3)
(bbdb-expire-initialize)

(eval-after-load 'gnus '(bbdb-initialize 'gnus))
(eval-after-load 'message '(bbdb-initialize 'message))

(provide 'init-bbdb)
