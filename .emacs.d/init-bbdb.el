;;;;
;;;; BBDB
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;
;;; TODO
;;; * safer sync with google (merge in google contacts)
;;; * two way sync?
;;;
(after 'bbdb 

  (define-key bbdb-mode-map "r" 'bbdb-merge-records)

  (setq bbdb-pop-up-window-size 10
	bbdb-mua-update-interactive-p '(query . create)
	bbdb-message-all-addresses t
	bbdb-complete-mail-allow-cycling t)
  
  (after 'supercite 
    (bbdb-initialize 'sc))
  (after 'gnus 
    (bbdb-initialize 'gnus)
    (bbdb-mua-auto-update-init 'gnus))
  (after 'message
    (bbdb-initialize 'message)
    (bbdb-mua-auto-update-init 'message)
    (add-hook 'message-setup-hook 'bbdb-mail-aliases)))
