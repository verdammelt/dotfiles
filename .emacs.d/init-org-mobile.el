;;;;
;;;; Org Mobile Setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Modified Time-stamp: <2013-04-16 23:35:44 mark>
;;;;
(require 'org-mobile)

(setq org-mobile-directory (expand-file-name "~/Dropbox/GTD/MobileOrg"))
(setq org-mobile-inbox-for-pull "~/Documents/GTD/todo.org")

;;;
;;; auto pull / push
;;;

;; push on save
(add-hook 'after-save-hook 
 (lambda () 
   (when (eq major-mode 'org-mode)
     (dolist (file (org-mobile-files-alist))
       (if (string= (expand-file-name (car file)) (buffer-file-name))
           (org-mobile-push-with-delay 5))))))


;;
;; timer code for pushing/pushing
;;
(defvar org-mobile-push-timer nil
  "Timer that `org-mobile-push-timer' used to reschedule itself, or nil.")

(defun org-mobile-push-with-delay (secs &optional repeat)
  (when org-mobile-push-timer (cancel-timer org-mobile-push-timer))
  (setq org-mobile-push-timer
        (run-with-idle-timer (* 1 secs) repeat 'org-mobile-push)))

;; (defvar org-mobile-pull-timer nil
;;   "Timer that `org-mobild-pull-timer' used to reschedule itself, or nil.")

;; (defun org-mobile-pull-with-delay (secs &optional repeat)
;;   (when org-mobile-pull-timer (cancel-timer org-mobile-pull-timer))
;;   (setq org-mobile-pull-timer
;;         (run-with-idle-timer (* 1 secs) repeat 'org-mobile-pull)))

;;
;; push and pull every time we have 120 seconds idle
;;
;; (org-mobile-push-with-delay 120 :repeat)
;; (org-mobile-pull-with-delay 120 :repeat)

(provide 'init-org-mobile)
