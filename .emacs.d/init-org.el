;;;;
;;;; Org Setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Modified Time-stamp: <2014-06-25 21:37:12 mark>
;;;;
(defun mjs/expand-org-file (f)
  (let ((filename (if (string= (file-name-extension f) "org") 
		      f
		    (format "%s.org" f))))
    (expand-file-name filename org-directory)))

(after 'org
  (setq org-id-locations-file 
	(expand-file-name ".org-id-locations" user-emacs-directory))
  
  (setq org-directory (expand-file-name "~/Documents/GTD")
	org-default-notes-file (mjs/expand-org-file "inbox")
	org-use-property-inheritance t
	org-log-done t
	org-enable-priority-commands nil
	org-link-mailto-program '(compose-mail "%a" "%s")
	org-special-ctrl-a/e t
	org-yank-adjusted-subtrees t
	org-special-ctrl-k t

	org-completion-use-ido t
	org-outline-path-complete-in-steps nil
	org-todo-keywords '((sequence "TODO(t)" "WAITING(w!)" "|" 
				      "DONE(d!/@)" "CANCELLED(c@/@)"))
	org-tag-alist '(("@HOME" . ?h)
			("@CALL" . ?c) ("@ERRAND" . ?e)
			("@MAC" . ?m) ("@WORKMAC" . ?a) ("@WEB" . ?b)
			("@WORK" . ?k) ("2U" . ?2)
			("@WENDY" . ?w))

	org-goto-interface 'outline-path-completion

	org-refile-allow-creating-parent-nodes 'confirm
	org-refile-use-outline-path 'file
	org-refile-use-cache t
	org-refile-targets `((,(mapcar #'mjs/expand-org-file '("todo" "work" "somedaymaybe"))
			      :maxlevel . 9)))

  (setq org-clock-persist t
	org-clock-idle-time 10
	org-time-clocksum-format 
	'(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
	org-clock-persist-file (expand-file-name 
				".org-clock-save.el" 
				user-emacs-directory)
	org-clock-out-remove-zero-time-clocks t
	org-clock-report-include-clocking-task t
	org-agenda-clockreport-parameter-plist 
	'(:link t :maxlevel 2 :fileskip0 t :compact t :narrow 80)
	org-agenda-clock-consistency-checks 
	'(:max-duration "04:00" 
			:min-duration 0 
			:max-gap 0
			:gap-ok-around ("04:00" "13:00");; what are good settings?
			;; below is same as default
			:default-face
			((:background "DarkRed")
			 (:foreground "white"))
			:overlap-face nil :gap-face nil :no-end-time-face nil :long-face nil :short-face nil))

  (org-clock-persistence-insinuate)

  (setq org-capture-templates
	`(("t" "Task" entry (file "" "Tasks")
	   "* TODO %?\n  %U\n"
	   :empty-lines-after 1)
	  ("p" "Project" entry (file "" "Projects")
	   "* %? \n %U\n" 
	   :jump-to-captured t :empty-lines-after 1)
	  ("k" "Tickler" entry (file+headline "todo.org" "Tickler")
	   "* TODO %?\n  %U\n  %a\n")
	  ("n" "Note" entry (file "")
	   "* %?\n %U\n %a")
	  ("s" "Someday/Maybe" entry (file ,(mjs/expand-org-file "somedaymaybe"))
	   "* %?\n  %U\n %a\n")
	  ("w" "Templates for work")
	  ("wb" "Billable Task" entry (file "")
	   "* TODO %? :2U:@WORK:\n %U\n"
	   :clock-in t :clock-resume t)
	  ("ww" "Non-Billable Task" entry (file "")
	   "* TODO %? :@WORK:\n %U\n"
	   :clock-in t :clock-resume t)
	  ("wp" "Work Project" entry (file "")
	   "* %? :@WORK:\n %U\n"
	   :clock-in t :clock-resume t)
	  ))
  (add-hook 'org-capture-mode-hook 'turn-on-auto-fill)

  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'flyspell-mode)

  (setq org-fontify-done-headline t)
  (set-face-attribute 'org-done nil :strike-through t)
  (set-face-attribute 'org-headline-done nil :strike-through t)

  (after 'org-agenda
    (setq 
     org-agenda-tags-todo-honor-ignore-options t
     org-agenda-todo-ignore-scheduled 'future
     org-agenda-todo-ignore-deadlines 'far
     
     org-agenda-sorting-strategy 
     '((agenda habit-up time-up tag-up todo-state-up deadline-down alpha-up) 
       (todo todo-state-up tag-up alpha-up)
       (tags todo-state-up tag-up alpha-up)
       (search todo-state-up))
     org-agenda-files (mapcar #'mjs/expand-org-file '("todo" "work" "inbox"))
     org-agenda-start-on-weekday nil
     org-agenda-block-separator "==========================================================================="
     org-agenda-custom-commands 
     '(("d" "daily"
	((agenda "" ((org-agenda-span 'day)
		     (org-agenda-use-time-grid nil)))
	 (tags "REFILE"
	       ((org-agenda-overriding-header "Tasks to Refile")))
	 (tags-todo "+@CALL|+@ERRAND/!-WAITING"
		    ((org-agenda-overriding-header "@CALL/@ERRAND")))
	 (tags-todo "+@MAC|+@WORKMAC|+@WEB/!-WAITING"
		    ((org-agenda-overriding-header "@COMPUTER")))
	 (tags-todo "+@HOME|+@ANY/!-WAITING"
		    ((org-agenda-overriding-header "@HOME")))
	 (tags-todo "+@WENDY/!-WAITING"
		    ((org-agenda-overriding-header "@WENDY")))
	 (tags-todo "+@WORK/!-WAITING"
		    ((org-agenda-overriding-header "@WORK")))
	 (tags-todo "/WAITING"
		    ((org-agenda-overriding-header "WAITING-FOR")))
	 (tags "+CATEGORY=\"PROJ\"&+LEVEL=2&-TODO=\"DONE\""
	       ((org-agenda-overriding-header "PROJECTS")
		(org-agenda-sorting-strategy '(category-keep))))
	 ))
       ("w" "waiting" tags-todo "/WAITING")
       ("k" "work" 
	((agenda "" ((org-agenda-span 'day)
		     (org-agenda-use-time-grid t)))
	 (tags "REFILE"
	       ((org-agenda-overriding-header "Tasks to Refile")))
	 (tags-todo "+@WORK&+2U/!-WAITING" 
		    ((org-agenda-sorting-strategy '(todo-state-up tag-up))
		     (org-agenda-overriding-header "BILLABLE")))
	 (tags-todo "+@WORK&-2U/!-WAITING" 
		    ((org-agenda-sorting-strategy '(todo-state-up tag-up))
		     (org-agenda-overriding-header "NON-BILLABLE")))
	 (tags-todo "+@WORK/WAITING"
		    ((org-agenda-overriding-header "WAITING-FOR")))
	 (tags "+@WORK&+CATEGORY=\"PROJ\"&+LEVEL=2"
	       ((org-agenda-overriding-header "PROJECTS")
		(org-agenda-sorting-strategy '(todo-state-down))))))))

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
    (define-key org-agenda-mode-map "X" 'sacha/org-agenda-mark-done-and-add-followup)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ct" (lambda () (interactive) (org-capture nil "t")))
(global-set-key "\C-cp" (lambda () (interactive) (org-capture nil "p")))
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "C-c s-s") 'org-save-all-org-buffers)
(global-set-key (kbd "C-c s-u") 'org-revert-all-org-buffers)

;; experimental stuff
(after 'org
  (setq mjs/default-task-id "963F688C-0EAD-4217-B84E-DDA7D94C0453"
	mjs/keep-clock-running nil)
  (defun mjs/punch-in ()
    (interactive)
    (setq mjs/keep-clock-running t)
    (mjs/clock-in-default-task)
    (message "Mornin' Sam"))
  (defun mjs/punch-out ()
    (interactive)
    (setq mjs/keep-clock-running nil)
    (when (org-clock-is-active)
      (org-clock-out))
    (message "Nice day eh Ralph?"))
  (defun mjs/clock-in-default-task ()
    (interactive)
    (org-with-point-at (org-id-find mjs/default-task-id 'marker)
      (org-clock-in '(16))))
  (defun mjs/clock-out-maybe ()
    (when (and mjs/keep-clock-running
	       (not org-clock-clocking-in)
	       (marker-buffer org-clock-default-task)
	       (not org-clock-resolving-clocks-due-to-idleness))
      (mjs/clock-in-default-task)))
  (add-hook 'org-clock-out-hook 'mjs/clock-out-maybe 'append)

  (defun mjs/morning-sam ()
    (org-agenda nil "k")
    (mjs/punch-in))

  (global-set-key (kbd "<f9>") 'mjs/morning-sam)
  (global-set-key (kbd "S-<f9>") 'mjs/punch-out)

  (setq org-stuck-projects
	'("+CATEGORY=\"PROJ\"+LEVEL=2&-TODO=\"DONE\"" (TODO WAITING) nil "")))

(provide 'init-org)
