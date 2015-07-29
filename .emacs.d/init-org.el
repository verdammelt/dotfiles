;;;;
;;;; Org Setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(defun mjs/expand-org-file (f)
  (let ((filename (if (string= (file-name-extension f) "org")
                      f
                    (format "%s.org" f))))
    (expand-file-name filename org-directory)))

(with-eval-after-load 'org
  (require 'org-checklist)
  (setq org-id-locations-file
        (expand-file-name ".org-id-locations" user-emacs-directory))

  (setq org-directory (expand-file-name "~/Documents/GTD")
        org-default-notes-file (mjs/expand-org-file "inbox")
        org-use-property-inheritance t
        org-log-done 'time
        org-log-into-drawer t
        org-treat-S-cursor-todo-selection-as-state-change nil

        org-hide-leading-stars nil
        org-startup-indented t

        org-enable-priority-commands nil

        org-link-mailto-program '(compose-mail "%a" "%s")

        org-special-ctrl-a/e t
        org-yank-adjusted-subtrees t
        org-special-ctrl-k t

        org-completion-use-ido t
        org-outline-path-complete-in-steps nil
        org-todo-keywords '((sequence "TODO(t)" "WAIT(w!)" "BLKD(b@/!)" "|"
                                      "DONE(d!/@)" "CNCL(c@/@)"))
        org-tag-alist '(("@HOME" . ?h)
                        ("@CALL" . ?c) ("@EMAIL" . ?e) ("@ERRAND" . ?r)
                        ("@MAC" . ?m) ("@WORKMAC" . ?a) ("@WEB" . ?b)
                        ("@WORK" . ?k) ("@CLIENT" . ?l)
                        ("@WENDY" . ?w))

        org-goto-interface 'outline-path-completion

        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-use-outline-path 'file
        org-refile-use-cache t
        org-refile-targets `((,(mapcar #'mjs/expand-org-file '("todo" "work" "somedaymaybe"))
                              :maxlevel . 9)))

  (setq org-clock-persist t
        org-clock-idle-time 10
        org-clock-mode-line-total 'today
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
                        :gap-ok-around ("04:00" "09:00" "13:00" "17:00");; what are good settings?
                        ;; below is same as default
                        :default-face
                        ((:background "DarkRed")
                         (:foreground "white"))
                        :overlap-face nil :gap-face nil :no-end-time-face nil :long-face nil :short-face nil))

  (org-clock-persistence-insinuate)

  (setq org-capture-templates
        `(("t" "Task" entry (file "" "Tasks")
           "* TODO %? %^G\n  %U\n %a\n"
           :empty-lines-after 1)
          ("k" "Tickler" entry (file+headline "todo.org" "Tickler")
           "* TODO %? %^G\n  %U\n  %a\n")
          ("s" "Someday/Maybe" entry (file ,(mjs/expand-org-file "somedaymaybe"))
           "* %?\n  %U\n %a\n")
          ("w" "Templates for work")
          ("wb" "Billable Task" entry (file+headline "work.org" "Tasks")
           "* TODO %? %^g:@CLIENT:\n %U\n %a\n"
           :clock-in t :clock-resume t)
          ("ww" "Non-Billable Task" entry (file+headline "work.org" "Tasks")
           "* TODO %? %^g\n %U\n %a\n"
           :clock-in t :clock-resume t)
          ("wi" "Interruption" entry (file+headline "work.org" "Tasks")
           "* TODO %? %^g\n %U\n %a\n"
           :clock-in t :clock-keep t :jump-to-captured t)
          ("wn" "New Task to clocked" entry (clock)
           "* TODO %? %^g\n %U\n %a\n")
          ))
  (add-hook 'org-capture-mode-hook 'turn-on-auto-fill)

  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'flyspell-mode)

  (setq org-fontify-done-headline t)
  (set-face-attribute 'org-done nil :strike-through t)
  (set-face-attribute 'org-headline-done nil :strike-through t)

  (with-eval-after-load 'org-agenda
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
         (tags-todo "+@CALL|+@EMAIL/!-WAIT"
                    ((org-agenda-overriding-header "@COMMUNICATE")))
         (tags-todo "+@ERRAND/!-WAIT"
                    ((org-agenda-overriding-header "@ERRAND")))
         (tags-todo "+@MAC|+@WORKMAC|+@WEB/!-WAIT"
                    ((org-agenda-overriding-header "@COMPUTER")))
         (tags-todo "+@HOME|+@ANY/!-WAIT"
                    ((org-agenda-overriding-header "@HOME")))
         (tags-todo "+@WENDY/!-WAIT"
                    ((org-agenda-overriding-header "@WENDY")))
         (tags-todo "+@WORK/!-WAIT"
                    ((org-agenda-overriding-header "@WORK")))
         (tags-todo "/WAIT"
                    ((org-agenda-overriding-header "WAITING-FOR")))
         (tags "+CATEGORY=\"PROJ\"&+LEVEL=2&-TODO=\"DONE\""
               ((org-agenda-overriding-header "PROJECTS")
                (org-agenda-sorting-strategy '(category-keep))))
         ))
       ("h" "Home"
        ((agenda "" ((org-agenda-span 'day)
                     (org-agenda-use-time-grid nil)))
         (tags "REFILE"
               ((org-agenda-overriding-header "Tasks to Refile")))
         (tags-todo "+@HOME|+@ANY/!-WAIT"
                    ((org-agenda-overriding-header "@HOME")))
         (tags-todo "+@CALL|@EMAIL/!-WAIT"
                    ((org-agenda-overriding-header "@COMMUNICATE")))
         (tags-todo "+@MAC|+@WORKMAC|+@WEB/!-WAIT"
                    ((org-agenda-overriding-header "@COMPUTER")))
         (tags-todo "+@WENDY/!-WAIT"
                    ((org-agenda-overriding-header "@WENDY")))
         (tags-todo "-@WORK/WAIT"
                    ((org-agenda-overriding-header "WAITING-FOR")))
         (tags "+CATEGORY=\"PROJ\"&+LEVEL=2&-TODO=\"DONE\""
               ((org-agenda-overriding-header "PROJECTS")
                (org-agenda-sorting-strategy '(category-keep))))
         ))
       ("w" "waiting" tags-todo "/WAIT")
       ("k" "work"
        ((agenda "" ((org-agenda-span 'day)
                     (org-agenda-use-time-grid t)
                     (org-agenda-start-with-log-mode t)
                     (org-agenda-start-with-clockreport-mode t)))
         (tags "REFILE"
               ((org-agenda-overriding-header "Tasks to Refile")))
         (tags-todo "+@WORK&+@CLIENT/!-WAIT"
                    ((org-agenda-sorting-strategy '(todo-state-up tag-up))
                     (org-agenda-overriding-header "BILLABLE")))
         (tags-todo "+@WORK&-@CLIENT/!-WAIT"
                    ((org-agenda-sorting-strategy '(todo-state-up tag-up))
                     (org-agenda-overriding-header "NON-BILLABLE")))
         (tags-todo "+@WORK/WAIT"
                    ((org-agenda-overriding-header "WAITING-FOR")))
         (tags "+@WORK&+CATEGORY=\"PROJ\"&+LEVEL=2"
               ((org-agenda-overriding-header "PROJECTS")
                (org-agenda-sorting-strategy '(todo-state-down))))))))
    ))

;; experimental stuff
(with-eval-after-load 'org
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
    (interactive)
    (org-agenda nil "k")
    (mjs/punch-in))

  (setq org-stuck-projects
        '("+CATEGORY=\"PROJ\"+LEVEL=2&-TODO=\"DONE\"" ("TODO" "WAIT") nil ""))

  (require 'org-mobile)
  (setq org-mobile-directory "~/Documents/Dropbox/GTD/MobileOrg"
        org-mobile-inbox-for-pull (expand-file-name "mobileorg.org" org-mobile-directory)
        )

  (fullframe org-agenda org-agenda-quit))


(global-set-key (kbd "C-c a")  'org-agenda)
(global-set-key (kbd "C-c b")  'org-iswitchb)
(global-set-key (kbd "C-c c")  'org-capture)
(global-set-key (kbd "C-c l")  'org-store-link)
(global-set-key (kbd "C-c s-s") 'org-save-all-org-buffers)
(global-set-key (kbd "C-c s-u") 'org-revert-all-org-buffers)

(global-set-key (kbd "<f9>") 'mjs/morning-sam)
(global-set-key (kbd "C-<f9>") 'org-clock-jump-to-current-clock)
(global-set-key (kbd "S-<f9>") 'mjs/punch-out)
