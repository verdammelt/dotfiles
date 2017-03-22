;;;;
;;;; BBDB
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;
;;; TODO
;;; * safer sync with google (merge in google contacts)
;;; * two way sync?
;;; * expiry
;;;
(use-package bbdb
  :bind (:map bbdb-mode-map
              ("r" . bbdb-merge-records))
  :config
  (progn
    (declare-function bbdb-save "bbdb")
    (add-hook 'bbdb-after-change-hook (lambda (arg) (bbdb-save)))
    (setq bbdb-pop-up-window-size 10
          bbdb-mua-pop-up-window-size 5
          bbdb-mua-update-interactive-p '(query . create)
          bbdb-message-all-addresses t
          bbdb-complete-mail-allow-cycling t)))

(defun mjs/bbdb-init (package)
  (message "Initialize BBDB for %s" package)
  (bbdb-initialize package)
  (bbdb-mua-auto-update-init package))
