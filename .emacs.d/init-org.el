;;;;
;;;; Org Setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(defun mjs/expand-org-file (f)
  (cond ((file-exists-p (expand-file-name f org-directory))
         (expand-file-name f org-directory))
        ((string= (file-name-extension f) "org")
         (expand-file-name f org-directory))
        (t
         (expand-file-name (format "%s.org" f) org-directory))))

(with-eval-after-load 'org
  (require 'org-checklist)

  (with-eval-after-load 'org-indent
    (diminish 'org-indent-mode))

  (setq org-id-locations-file
        (expand-file-name ".org-id-locations" user-emacs-directory))

  (setq org-directory (expand-file-name "~/Documents/GTD")
        org-default-notes-file (mjs/expand-org-file "inbox")
        org-use-property-inheritance t
        org-log-done 'time
        org-log-redeadline 'note
        org-log-reschedule 'note
        org-log-refile 'time
        org-log-repeat 'time
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
        org-todo-keywords '((sequence "TODO(t!)" "WAIT(w@/!)" "BLKD(b@/!)" "DOIN(i!)" "|"
                                      "DONE(d!/@)" "CNCL(c@/@)"))
        org-tag-alist '(("@HOME" . ?h)
                        ("@CALL" . ?c) ("@EMAIL" . ?e) ("@ERRAND" . ?r)
                        ("@MAC" . ?m) ("@WORKMAC" . ?a) ("@WEB" . ?b)
                        ("@WORK" . ?k) ("@CLIENT" . ?l)
                        ("@WENDY" . ?w))

        org-goto-interface 'outline-path-completion

        org-use-speed-commands t

        org-refile-allow-creating-parent-nodes 'confirm
        org-refile-use-outline-path 'file
        org-refile-use-cache t
        org-refile-targets `((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)
                             (,(mjs/expand-org-file "somedaymaybe") :maxlevel . 9)))

  (setq org-clock-persist t
        org-clock-idle-time 30
        org-clock-history-length 10
        org-clock-mode-line-total 'today
        org-time-clocksum-format
        '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
        org-clock-persist-file (expand-file-name
                                ".org-clock-save.el"
                                user-emacs-directory)
        org-clock-out-remove-zero-time-clocks t
        org-clock-report-include-clocking-task t
        org-agenda-clockreport-parameter-plist
        '(:link t :maxlevel 4 :fileskip0 t :compact t :narrow 80)
        org-agenda-clock-consistency-checks
        '(:max-duration "04:00"
                        :max-duration "04:00"
                        :min-duration 0
                        :max-gap 0
                        :gap-ok-around ("04:00" "09:00" "13:00" "18:00");; what are good settings?
                        :default-face
                        ((:background "DarkRed")
                         (:foreground "white"))
                        :overlap-face nil :gap-face nil
                        :no-end-time-face nil
                        :long-face nil :short-face nil))

  (org-clock-persistence-insinuate)

  (setq org-capture-templates
        `(("t" "Task" entry (file "")
           "* TODO %? %^G\n  %U\n %a\n"
           :clock-in t :clock-resume t
           :empty-lines-after 1)
          ("r" "Process email" entry (file "")
           "* TODO Process '%:subject' from '%:from' :@EMAIL:\nSCHEDULED: %t\n%U\n%a\n"
           :clock-in t :clock-resume t
           :immediate-finish t)
          ("n" "Take a note" entry (file "")
           "* %U %? :NOTE:\n%a\n"
           :clock-in t :clock-resume t)
          ("k" "Tickler" entry (file+headline ,(mjs/expand-org-file "todo.org") "Tickler")
           "* TODO %? %^G\n  %U\n  %a\n"
           :clock-in t :clock-resume t)
          ("s" "Someday/Maybe" entry (file ,(mjs/expand-org-file "somedaymaybe"))
           "* %{headline}\n  %U\n %a\n%?"
           :clock-in t :clock-resume t)
          ("w" "Templates for work")
          ("wb" "Billable Task" entry (file "")
           "* TODO %? %^g:@CLIENT:\n %U\n %a\n"
           :clock-in t :clock-resume t)
          ("ww" "Non-Billable Task" entry (file "")
           "* TODO %? %^g:@WORK:\n %U\n %a\n"
           :clock-in t :clock-resume t)
          ("wi" "Interruption" entry (file "")
           "* TODO %? %^g:@WORK:\n %U\n %a\n"
           :clock-in t :clock-keep t
           :jump-to-captured t)
          ("wn" "New Task to clocked" entry (clock)
           "* TODO %? %^g\n %U\n %a\n"
           :clock-in t :clock-resume t)
          ))
  (add-hook 'org-capture-mode-hook 'turn-on-auto-fill)

  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (add-hook 'org-mode-hook 'flyspell-mode)

  (setq org-fontify-done-headline t)
  (set-face-attribute 'org-done nil :strike-through t)
  (set-face-attribute 'org-headline-done nil :strike-through t)

  (with-eval-after-load 'org-agenda
    (setq
     org-agenda-files
     (mapcar #'mjs/expand-org-file '("todo" "work" "inbox"))

     org-agenda-sorting-strategy
     '((agenda time-up user-defined-up category-keep)
       (todo todo-state-up timestamp-up alpha-up)
       (tags todo-state-up timestamp-up alpha-up)
       (search todo-state-up timestamp-up alpha-up))

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
         (tags-todo "+@HOME|+@ANY/!-WAIT"
                    ((org-agenda-overriding-header "@HOME")))
         (tags-todo "+@MAC&-@WEB&-@WORK/!-WAIT"
                    ((org-agenda-overriding-header "@COMPUTER")))
         (tags-todo "+@WEB&-@WORK/!-WAIT"
                    ((org-agenda-overriding-header "@WEB")))
         (tags-todo "+@WENDY/!-WAIT"
                    ((org-agenda-overriding-header "@WENDY")))
         (tags-todo "+@WORK|+@WORKMAC/!-WAIT"
                    ((org-agenda-overriding-header "@WORK")))
         (tags-todo "/WAIT"
                    ((org-agenda-overriding-header "WAITING-FOR")))))
       ("k" "work"
        ((agenda "" ((org-agenda-span 'day)
                     (org-agenda-use-time-grid t)
                     (org-agenda-start-with-log-mode t)
                     (org-agenda-start-with-clockreport-mode t)))
         (tags "REFILE"
               ((org-agenda-overriding-header "Tasks to Refile")))
         (tags-todo "+@WORK&+@CLIENT/!-WAIT"
                    ((org-agenda-overriding-header "BILLABLE")))
         (tags-todo "+@WORK&-@CLIENT/!-WAIT"
                    ((org-agenda-overriding-header "NON-BILLABLE")))
         (tags-todo "+@WORK/WAIT"
                    ((org-agenda-overriding-header "WAITING-FOR")))))
       ("x" "lost tasks" tags-todo "-{^@}")
       ("w" "waiting" tags-todo "/WAIT")
       ("p" "projects" tags "+PROJECT=\"TRUE\"+LEVEL=2"))

     org-stuck-projects
     '("+PROJECT=\"TRUE\"+LEVEL=2" ("TODO" "WAIT") nil "")

     org-agenda-tags-todo-honor-ignore-options t
     org-agenda-todo-ignore-scheduled 'future
     org-agenda-todo-ignore-deadlines 'far
     org-agenda-start-on-weekday 0
     org-agenda-compact-blocks t)))

(with-eval-after-load 'org-clock
  (setq
   org-clocktable-defaults (plist-put org-clocktable-defaults :stepskip0 t)
   org-clocktable-defaults (plist-put org-clocktable-defaults :fileskip0 t)
   org-clocktable-defaults (plist-put org-clocktable-defaults :wstart0 0)
   org-clocktable-defaults (plist-put org-clocktable-defaults :link t)
   org-clocktable-defaults (plist-put org-clocktable-defaults :compact t)
   org-clocktable-defaults (plist-put org-clocktable-defaults :maxlevel 9)))

;; clocking setup
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
      (org-clock-out nil t))
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

  ;;
  ;; Agenda sorting functions
  ;;
  (setq org-agenda-cmp-user-defined 'bh/agenda-sort)

  ;;
  ;; ====================
  ;;
  ;; From http://doc.norang.ca/org-mode.html
  ;;
  (defun bh/agenda-sort (a b)
    "Sorting strategy for agenda items.
Late deadlines first, then scheduled, then non-late deadlines"
    (let (result num-a num-b)
      (cond
       ;; time specific items are already sorted first by org-agenda-sorting-strategy
       ;; non-deadline and non-scheduled items next
       ((bh/agenda-sort-test 'bh/is-not-scheduled-or-deadline a b))

       ;; late deadlines next
       ((bh/agenda-sort-test-num 'bh/is-late-deadline '> a b))

       ;; deadlines for today next
       ((bh/agenda-sort-test 'bh/is-due-deadline a b))

       ;; scheduled items for today next
       ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

       ;; late scheduled items next
       ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

       ;; pending deadlines last
       ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

       ;; finally default to unsorted
       (t (setq result nil)))
      result))

  (defmacro bh/agenda-sort-test (fn a b)
    "Test for agenda sort"
    `(cond
      ;; if both match leave them unsorted
      ((and (apply ,fn (list ,a))
            (apply ,fn (list ,b)))
       (setq result nil))
      ;; if a matches put a first
      ((apply ,fn (list ,a))
       (setq result -1))
      ;; otherwise if b matches put b first
      ((apply ,fn (list ,b))
       (setq result 1))
      ;; if none match leave them unsorted
      (t nil)))

  (defmacro bh/agenda-sort-test-num (fn compfn a b)
    `(cond
      ((apply ,fn (list ,a))
       (setq num-a (string-to-number (match-string 1 ,a)))
       (if (apply ,fn (list ,b))
           (progn
             (setq num-b (string-to-number (match-string 1 ,b)))
             (setq result (if (apply ,compfn (list num-a num-b))
                              -1
                            1)))
         (setq result -1)))
      ((apply ,fn (list ,b))
       (setq result 1))
      (t nil)))

  (defun bh/is-not-scheduled-or-deadline (date-str)
    (and (not (bh/is-deadline date-str))
         (not (bh/is-scheduled date-str))))

  (defun bh/is-due-deadline (date-str)
    (string-match "Deadline:" date-str))

  (defun bh/is-late-deadline (date-str)
    (string-match "\\([0-9]*\\) d\. ago:" date-str))

  (defun bh/is-pending-deadline (date-str)
    (string-match "In \\([^-]*\\)d\.:" date-str))

  (defun bh/is-deadline (date-str)
    (or (bh/is-due-deadline date-str)
        (bh/is-late-deadline date-str)
        (bh/is-pending-deadline date-str)))

  (defun bh/is-scheduled (date-str)
    (or (bh/is-scheduled-today date-str)
        (bh/is-scheduled-late date-str)))

  (defun bh/is-scheduled-today (date-str)
    (string-match "Scheduled:" date-str))

  (defun bh/is-scheduled-late (date-str)
    (string-match "Sched\.\\(.*\\)x:" date-str))
  ;;
  ;; ====================
  ;;

  (defun mjs/insert-heading-inactive-timestamp ()
    (save-excursion
      (org-return)
      (org-cycle)
      (org-insert-time-stamp nil t t nil nil nil)))

  (add-hook 'org-insert-heading-hook 'mjs/insert-heading-inactive-timestamp 'append)

  (fullframe org-agenda org-agenda-quit))

(with-eval-after-load 'org-mobile
  (setq org-mobile-directory (expand-file-name "MobileOrg" org-directory)
        org-mobile-inbox-for-pull (mjs/expand-org-file "inbox")))

(global-set-key (kbd "C-c a")  'org-agenda)
(global-set-key (kbd "C-c b")  'org-iswitchb)
(global-set-key (kbd "C-c c")  'org-capture)
(global-set-key (kbd "C-c l")  'org-store-link)
(global-set-key (kbd "C-c s-s") 'org-save-all-org-buffers)
(global-set-key (kbd "C-c s-u") 'org-revert-all-org-buffers)

(global-set-key (kbd "<f9>") #'(lambda ()
                                 (interactive)
                                 (if (eq major-mode 'org-agenda-mode)
                                     (org-agenda-clock-in)
                                   (org-clock-in))))
(global-set-key (kbd "S-<f9>") #'(lambda () (interactive) (org-clock-out nil t)))
(global-set-key (kbd "M-<f9>") #'(lambda () (interactive) (org-clock-in '(4))))
(global-set-key (kbd "C-<f9>") 'org-clock-jump-to-current-clock)
(global-set-key (kbd "s-<f9>") 'mjs/morning-sam)
(global-set-key (kbd "S-s-<f9>") 'mjs/punch-out)
