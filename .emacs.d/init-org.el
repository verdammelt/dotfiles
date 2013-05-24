;;;;
;;;; Org Setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Modified Time-stamp: <2013-05-24 18:29:03 mark>
;;;;
(after 'org
  (setq org-id-locations-file 
	(expand-file-name ".org-id-locations" user-emacs-directory))
  
  (setq org-directory (expand-file-name "~/Documents/GTD")
	org-default-notes-file "~/Documents/GTD/todo.org"  
	mjs-someday-maybe-file "~/Documents/GTD/somedaymaybe.org"
	org-use-property-inheritance t
	org-log-done t

	org-completion-use-ido t
	org-outline-path-complete-in-steps nil
	org-todo-keywords '((sequence "TODO(t)" "INPROGRESS(i)" 
				      "WAITING(w)" "|" 
				      "DONE(d)" "CANCELLED(c)"))
	org-tag-alist '(("@HOME" . ?h) ("@WORK" . ?w) ("@MAC" . ?m) 
			("@CALL" . ?c) ("@ERRAND" . ?e) ("@WEB" . ?b) 
			("@WENDY" . ?y))
	org-refile-allow-creating-parent-nodes 'confirm
	org-refile-use-outline-path 'file
	org-refile-use-cache t
	org-refile-targets `((,(list org-default-notes-file 
				     mjs-someday-maybe-file) 
			      :maxlevel . 9)))

  (setq org-capture-templates
	`(("t" "Task" entry (file+headline "" "Tasks")
	   "* TODO %?\n  %U\n  %a\n")
	  ("k" "Tickler" entry (file+headline "" "Tickler")
	   "* TODO %?\n  %U\n  %a\n")
	  ("n" "Note" entry (file+headline "" "Catpure / Notes")
	   "* %?\n %U\n %a")
	  ("s" "Someday/Maybe" entry (file ,mjs-someday-maybe-file)
	   "* %?\n  %U\n %a\n")))
  (add-hook 'org-capture-mode-hook 'turn-on-auto-fill)

  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'flyspell-mode)

  (setq org-fontify-done-headline t)
  (set-face-attribute 'org-done nil :strike-through t)
  (set-face-attribute 'org-headline-done nil :strike-through t)

  (after 'org-agenda
    (setq org-agenda-sorting-strategy 
	  '((agenda habit-up time-up tag-up todo-state-up category-keep) 
	    (todo todo-state-up tag-up category-keep)
	    (tags todo-state-up tag-up category-keep)
	    (search todo-state-up category-keep))
	  org-agenda-files '("~/Documents/GTD/todo.org")
	  org-agenda-start-on-weekday nil
	  org-agenda-custom-commands 
	  '(("gW" "Office & Work lists"
	     ((tags-todo "+@WORK")))
	    ("ge" "Errands and Calls"
	     ((tags-todo "+@ERRAND")
	      (tags-todo "+@CALL")))
	    ("gb" "Web"
	     ((tags-todo "+@WEB")))
	    ("gh" "Home Lists"
	     ((tags-todo "-@WORK")))
	    ("gt" "Today's TICKLER" tags 
	     "+TODO=\"TODO\"+CATEGORY=\"TICKLER\"+SCHEDULED<=\"<today>\"+LEVEL=2")
	    ("gw" "Waiting"
	     ((todo "WAITING")))))

    ;; testing these out
    (defun sacha/org-agenda-done (&optional arg)
      "Mark current TODO as done.
This changes the line at point, all other lines in the agenda referring to
the same tree node, and the headline of the tree node in the Org-mode file."
      (interactive "P")
      (org-agenda-todo "DONE"))
    
    (defun sacha/org-agenda-mark-done-and-add-followup ()
      "Mark the current TODO as done and add another task after it.
Creates it at the same level as the previous task, so it's better to use
this with to-do items than with projects or headings."
      (interactive)
      (org-agenda-todo "DONE")
      (org-agenda-switch-to)
      (org-up-element)
      (open-line 1)
    (org-capture 0 "t"))
    
    (define-key org-agenda-mode-map "x" 'sacha/org-agenda-done)
    (define-key org-agenda-mode-map "X" 'sacha/org-agenda-mark-done-and-add-followup))
  
  ;;; Mobile Setup
  (after 'org-mobile
    (setq org-mobile-directory (expand-file-name "~/Dropbox/GTD/MobileOrg"))
    (setq org-mobile-inbox-for-pull "~/Documents/GTD/todo.org")
    ;; push on save
    (add-hook 'after-save-hook 
	      (lambda () 
		(when (eq major-mode 'org-mode)
		  (dolist (file (org-mobile-files-alist))
		    (if (string= (expand-file-name (car file)) (buffer-file-name))
			(org-mobile-push-with-delay 5))))))

    (defvar org-mobile-push-timer nil
  "Timer that `org-mobile-push-timer' used to reschedule itself, or nil.")
    
    (defun org-mobile-push-with-delay (secs &optional repeat)
      (when org-mobile-push-timer (cancel-timer org-mobile-push-timer))
      (setq org-mobile-push-timer
	    (run-with-idle-timer (* 1 secs) repeat 'org-mobile-push)))
    ))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ct" (lambda () (interactive) (org-capture nil "t")))
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(provide 'init-org)
