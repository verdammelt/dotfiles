;;;;
;;;; BBDB
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Modified Time-stamp: <2013-05-24 20:42:07 mark>
;;;;
;;;
;;; TODO
;;; * v3 upgrade
;;; * safer sync with google (merge in google contacts)
;;; * two way sync?
;;;
(add-to-list 'load-path (locate-user-emacs-file "lisp/bbdb/lisp"))

(require 'bbdb-loaddefs)

(after 'bbdb 
  
  (setq bbdb-pop-up-window-size 10)

  (define-key bbdb-mode-map "r" 'bbdb-merge-records)

  (setq bbdb-mua-update-interactive-p '(query . create))
  (setq bbdb-message-all-addresses t)
  
  (after 'supercite 
    (bbdb-initialize 'sc))
  (after 'gnus 
    (bbdb-initialize 'gnus)
    (bbdb-mua-auto-update-init 'gnus)
    )
  (after 'message
    (bbdb-initialize 'message)
    (bbdb-mua-auto-update-init 'message)
    (add-hook 'message-setup-hook 'bbdb-mail-aliases)))

(provide 'init-bbdb)
