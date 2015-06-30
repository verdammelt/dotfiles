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
(with-eval-after-load 'bbdb

  (define-key bbdb-mode-map "r" 'bbdb-merge-records)

  (setq bbdb-pop-up-window-size 10
        bbdb-mua-update-interactive-p '(query . create)
        bbdb-message-all-addresses t
        bbdb-complete-mail-allow-cycling t)

  (with-eval-after-load 'supercite
    (bbdb-initialize 'sc))
  (with-eval-after-load 'gnus
    (bbdb-initialize 'gnus)
    (bbdb-mua-auto-update-init 'gnus))
  (with-eval-after-load 'message
    (bbdb-initialize 'message)
    (bbdb-mua-auto-update-init 'message)
    (add-hook 'message-setup-hook 'bbdb-mail-aliases)))
