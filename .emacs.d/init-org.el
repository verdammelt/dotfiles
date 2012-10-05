;;;;
;;;; Org Setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Modified Time-stamp: <2012-10-04 06:48:39 mark>
;;;;
(require 'org)
(require 'org-mobile)

(setq org-id-locations-file 
      (expand-file-name ".org-id-locations" user-emacs-directory))

(setq org-directory (expand-file-name "~/Documents/GTD")
      org-default-notes-file "~/Documents/GTD/todo.org"  
      mjs-someday-maybe-file "~/Documents/GTD/somedaymaybe.org"
      org-use-property-inheritance t
      org-use-tag-inheritance t
      org-log-done t
      org-refile-allow-creating-parent-nodes 'confirm
      org-refile-use-outline-path 'file
      org-refile-targets `((,(list org-default-notes-file 
				   mjs-someday-maybe-file) 
			    :maxlevel . 9)))

(setq org-mobile-directory (expand-file-name "~/Dropbox/GTD/MobileOrg")
      org-mobile-inbox-for-pull org-default-notes-file)

(setq org-agenda-sorting-strategy 
      '((agenda habit-up time-up tag-up todo-state-up category-keep) 
	(todo todo-state-up tag-up category-keep)
	(tags todo-state-up tag-up category-keep)
	(search todo-state-up category-keep))
      org-agenda-show-all-dates nil
      org-agenda-files '("~/Documents/GTD/todo.org")
      org-agenda-show-all-dates nil
      org-agenda-tags-todo-honor-ignore-options t
      org-agenda-todo-ignore-scheduled t
      org-agenda-start-on-weekday 0
      org-agenda-custom-commands 
      '(("gW" "Office & Work lists"
	 ((agenda "")
	  (tags-todo "@WORK|@ERRAND|@CALL-@MAC|@WEB-@MAC")))
	("gh" "Home Lists"
	 ((agenda "") 
	  (tags-todo "-@WORK")))
	("gt" "Today's TICKLER" tags 
	 "+TODO=\"TODO\"+CATEGORY=\"TICKLER\"+SCHEDULED<=\"<today>\"+LEVEL=2")
	("gw" "Waiting"
	 ((todo "WAITING")))))

(setq org-capture-templates
      `(("t" "Task" entry (file+headline "" "Tasks")
	 "* TODO %?\n  %U\n  %a\n")
	("k" "Tickler" entry (file+headline "" "Tickler")
	 "* TODO %?\n  %U\n  %a\n")
	("n" "Note" entry (file+headline "" "Catpure / Notes")
	 "* %?\n %U\n %a")
	("s" "Someday/Maybe" entry (file ,mjs-someday-maybe-file)
	 "* %?\n  %U\n %a\n")))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ct" (lambda () (interactive) (org-capture nil "t")))
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

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
(defvar org-mobile-pull-timer nil
  "Timer that `org-mobild-pull-timer' used to reschedule itself, or nil.")

(defun org-mobile-push-with-delay (secs &optional repeat)
  (when org-mobile-push-timer (cancel-timer org-mobile-push-timer))
  (setq org-mobile-push-timer
        (run-with-idle-timer (* 1 secs) repeat 'org-mobile-push)))

(defun org-mobile-pull-with-delay (secs &optional repeat)
  (when org-mobile-pull-timer (cancel-timer org-mobile-pull-timer))
  (setq org-mobile-pull-timer
        (run-with-idle-timer (* 1 secs) repeat 'org-mobile-pull)))

;;
;; push and pull every time we have 60 seconds idel
(org-mobile-push-with-delay 120 :repeat)
(org-mobile-pull-with-delay 120 :repeat)

(provide 'init-org)
