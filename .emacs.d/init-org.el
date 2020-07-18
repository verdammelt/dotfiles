;;;;
;;;; Org Setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(defvar org-directory)
(defun mjs/expand-org-file (f)
  (cond ((file-exists-p (expand-file-name f org-directory))
         (expand-file-name f org-directory))
        ((string= (file-name-extension f) "org")
         (expand-file-name f org-directory))
        (t
         (expand-file-name (format "%s.org" f) org-directory))))

(use-package org
  :ensure org-plus-contrib
  :functions (org-return org-insert-time-stamp org-clock-is-active)
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ("C-c s-s" . org-save-all-org-buffers)
         ("C-c s-u" . org-revert-all-org-buffers)
         ("<f9>" . mjs/clock-in)
         ("S-<f9>" . mjs/clock-out)
         ("M-<f9>" . mjs/choose-clock-in)
         ("C-<f9>" . org-clock-goto)
         ("s-<f9>" . mjs/morning-sam)
         ("S-s-<f9>" . mjs/punch-out))

  :init
  (progn (add-hook 'org-mode-hook #'(lambda () (flycheck-mode 0)))
         (add-hook 'org-mode-hook 'turn-on-auto-fill)
         (add-hook 'org-mode-hook 'flyspell-mode)
         (add-hook 'org-insert-heading-hook
                   'mjs/insert-heading-inactive-timestamp 'append))

  :config
  (defun mjs/insert-heading-inactive-timestamp ()
    (save-excursion
      (org-return)
      (org-cycle)
      (org-insert-time-stamp nil t t nil nil nil)))

  (progn
    (add-to-list 'org-modules 'org-checklist)
    (add-to-list 'org-modules 'org-clock)

    (set-face-attribute 'org-done nil :strike-through t)
    (set-face-attribute 'org-headline-done nil :strike-through t)

    (setq org-directory (expand-file-name "~/Documents/Dropbox/GTD")
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

          org-fontify-done-headline t

          org-special-ctrl-a/e t
          org-yank-adjusted-subtrees t
          org-special-ctrl-k t

          org-outline-path-complete-in-steps nil
          org-todo-keywords
          '((sequence "TODO(t!)" "WAIT(w@/!)" "BLKD(b@/!)" "DOIN(i!)" "|"
                      "DONE(d!/@)" "CNCL(c@/@)"))
          org-tag-alist '(("@HOME" . ?h)
                          ("@CALL" . ?c) ("@EMAIL" . ?e) ("@SLACK" . ?s)
                          ("@ERRAND" . ?r)
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
                               (,(mjs/expand-org-file "somedaymaybe") :maxlevel . 9)))))

(use-package org-agenda
  :ensure org-plus-contrib
  :functions (org-agenda-quit)
  :config
  (progn
    (defvar mjs/skip-habits-and-scheduled-and-deadlines
      '(org-agenda-skip-if nil '(scheduled deadline)))

    (setq org-agenda-cmp-user-defined 'bh/agenda-sort

          org-agenda-files
          (mapcar #'mjs/expand-org-file '("todo" "work" "inbox" "inbox-mobile"
                                          "pokemongo" "fallenlondon"))

          org-agenda-sorting-strategy
          '((agenda time-up user-defined-up category-keep)
            (todo todo-state-up alpha-up)
            (tags todo-state-up alpha-up)
            (search todo-state-up alpha-up))

          org-agenda-clock-consistency-checks
          '(:max-duration "04:00"
                          :min-duration 0
                          :max-gap 0
                          :gap-ok-around ("04:00" "09:00" "13:00" "18:00") ;; what are good settings?
                          :default-face
                          ((:background "DarkRed")
                           (:foreground "white"))
                          :overlap-face nil :gap-face nil
                          :no-end-time-face nil
                          :long-face nil :short-face nil)

          org-stuck-projects
          '("+PROJECT=\"TRUE\"+LEVEL=2" ("TODO" "WAIT") nil "")

          org-agenda-clockreport-parameter-plist
          '(:link t :maxlevel 4 :fileskip0 t :compact t :narrow 80)

          org-agenda-start-on-weekday 0
          org-agenda-compact-blocks t
          org-agenda-follow-mode t
          org-agenda-include-diary t)

    (setq
     org-agenda-custom-commands
     '(("h" "home"
        ((agenda "" ((org-agenda-span 'day)
                     (org-agenda-use-time-grid nil)))
         (tags "REFILE"
               ((org-agenda-overriding-header "Tasks to Refile")))
         (tags-todo "+@CALL&-@WORK/!-WAIT"
                    ((org-agenda-skip-function
                      mjs/skip-habits-and-scheduled-and-deadlines)
                     (org-agenda-overriding-header "@CALL")))
         (tags-todo "+@EMAIL&-@WORK|+@SLACK&-@WORK/!-WAIT"
                    ((org-agenda-skip-function
                      mjs/skip-habits-and-scheduled-and-deadlines)
                     (org-agenda-overriding-header "@EMAIL|@SLACK")))
         (tags-todo "+@ERRAND&-@WORK/!-WAIT"
                    ((org-agenda-skip-function
                      mjs/skip-habits-and-scheduled-and-deadlines)
                     (org-agenda-overriding-header "@ERRAND")))
         (tags-todo "+@HOME|+@ANY&-@WORK|+@MAC|+@WORKMAC&-@WORK/!-WAIT"
                    ((org-agenda-skip-function
                      mjs/skip-habits-and-scheduled-and-deadlines)
                     (org-agenda-overriding-header "@HOME")))
         (tags-todo "+@WENDY/!-WAIT"
                    ((org-agenda-skip-function
                      mjs/skip-habits-and-scheduled-and-deadlines)
                     (org-agenda-overriding-header "@WENDY")))
         (tags-todo "+@WORK&-@CLIENT/!-WAIT"
                    ((org-agenda-skip-function
                      mjs/skip-habits-and-scheduled-and-deadlines)
                     (org-agenda-overriding-header "@WORK(non-client)")))
         (tags-todo "-@CLIENT/WAIT"
                    ((org-agenda-overriding-header "WAITING-FOR")))))
       ("k" "work"
        ((agenda "" ((org-agenda-span 'day)
                     (org-agenda-use-time-grid t)
                     (org-agenda-start-with-log-mode t)
                     (org-agenda-start-with-clockreport-mode t)))
         (tags "REFILE"
               ((org-agenda-overriding-header "Tasks to Refile")))
         (tags-todo "+@WORK&+@CLIENT/!-WAIT"
                    ((org-agenda-skip-function
                      mjs/skip-habits-and-scheduled-and-deadlines)
                     (org-agenda-overriding-header "BILLABLE")))
         (tags-todo "+@WORK&-@CLIENT|+@WORKMAC&-@CLIENT/!-WAIT"
                    ((org-agenda-skip-function
                      mjs/skip-habits-and-scheduled-and-deadlines)
                     (org-agenda-overriding-header "NON-BILLABLE")))
         (tags-todo "+@WORK|+@WORKMAC/WAIT"
                    ((org-agenda-overriding-header "WAITING-FOR")))))
       ("r" . "Review speciality agenda")
       ("rx" "lost tasks" tags-todo "-{^@}")
       ("rw" "waiting" tags-todo "/WAIT")
       ("rp" "projects" tags "+PROJECT=\"TRUE\"+LEVEL=2")
       ("rt" "loose todos" alltodo ""
        ((org-agenda-sorting-strategy
          '((tags time-up alpha-up)))))
       ("rn" "last week's notes" tags "+NOTE+TIMESTAMP_IA>\"<-8d>\"")))))

(use-package org-capture
  :ensure org-plus-contrib
  :init
  (add-hook 'org-capture-mode-hook 'turn-on-auto-fill)

  :config
  (setq org-capture-templates
        `(("t" "Task" entry (file "")
           "* TODO %?\n  %U\n %a\n"
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
           "* TODO %?\n  %U\n  %a\n"
           :clock-in t :clock-resume t)
          ("s" "Someday/Maybe" entry (file ,(mjs/expand-org-file "somedaymaybe"))
           "* %^{headline}\n  %U\n %a\n%?"
           :clock-in t :clock-resume t)
          ("w" "Templates for work")
          ("wb" "Billable Task" entry (file "")
           "* TODO %? :@CLIENT:\n %U\n %a\n"
           :clock-in t :clock-resume t)
          ("ww" "Non-Billable Task" entry (file "")
           "* TODO %? :@WORK:\n %U\n %a\n"
           :clock-in t :clock-resume t)
          ("wi" "Interruption" entry (file "")
           "* TODO %? :@WORK:\n %U\n %a\n"
           :clock-in t :clock-keep t
           :jump-to-captured t)
          ("wn" "New Task to clocked" entry (clock)
           "* TODO %?\n %U\n %a\n"
           :clock-in t :clock-resume t))
        org-capture-templates-contexts
        '(("r" ((in-mode . "message-mode")
                (in-mode . "gnus-summary-mode")
                (in-mode . "gnus-article-mode"))))))

(use-package org-checklist
  :ensure org-plus-contrib)

(use-package org-indent
  :ensure org-plus-contrib
  :diminish (org-indent-mode))

(use-package org-clock
  :ensure org-plus-contrib
  :functions (org-clock-in org-clock-out)
  :init
  (add-hook 'org-clock-out-hook 'mjs/clock-out-maybe 'append)

  :config
  (progn
    (declare-function org-agenda-clock-in "org-agenda")

    (defvar mjs/default-task-id "DEFAULT-TASK")
    (defvar mjs/keep-clock-running nil)

    (declare-function org-id-find "org-id")

    (setq org-clocktable-defaults (plist-put org-clocktable-defaults :stepskip0 t)
          org-clocktable-defaults (plist-put org-clocktable-defaults :fileskip0 t)
          org-clocktable-defaults (plist-put org-clocktable-defaults :wstart 0)
          org-clocktable-defaults (plist-put org-clocktable-defaults :link t))
    (setq org-clock-persist t
          org-clock-idle-time 30
          org-clock-history-length 10
          org-clock-mode-line-total 'today
          org-clock-persist-file (expand-file-name
                                  ".org-clock-save.el"
                                  user-emacs-directory)
          org-clock-out-remove-zero-time-clocks t
          org-clock-report-include-clocking-task t)
    (org-clock-persistence-insinuate)))

(use-package org-duration
  :ensure org-plus-contrib
  :config (setq org-duration-format `h:mm))

(use-package org-mobile
  :ensure org-plus-contrib
  :config
  (setq org-mobile-directory (expand-file-name "MobileOrg" org-directory)
        org-mobile-inbox-for-pull (mjs/expand-org-file "inbox")
        org-mobile-agendas 'default))

;;
;; ====================
;;
;; From http://doc.norang.ca/org-mode.html
;;
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

     ;; late scheduled items next
     ((bh/agenda-sort-test-num 'bh/is-scheduled-late '> a b))

     ;; deadlines for today next
     ((bh/agenda-sort-test 'bh/is-due-deadline a b))

     ;; scheduled items for today next
     ((bh/agenda-sort-test 'bh/is-scheduled-today a b))

     ;; pending deadlines last
     ((bh/agenda-sort-test-num 'bh/is-pending-deadline '< a b))

     ;; finally default to unsorted
     (t (setq result nil)))
    result))

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

(defun mjs/clock-in ()
  (interactive)
  (if (eq major-mode 'org-agenda-mode)
      (org-agenda-clock-in)
    (org-clock-in)))

(defun mjs/clock-out ()
  (interactive)
  (org-clock-out nil t))

(defun mjs/choose-clock-in ()
  (interactive)
  (org-clock-in '(4)))

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

(defun mjs/morning-sam ()
  (interactive)
  (org-agenda nil "k")
  (mjs/punch-in))
