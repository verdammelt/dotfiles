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

(defun mjs/save-org-buffers ()
  (interactive)
  (save-some-buffers t (lambda () (derived-mode-p 'org-mode))))

(use-package org
  :ensure nil
  :functions (org-return org-insert-time-stamp org-clock-is-active)
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         ;; ("C-c s-s" . org-save-all-org-buffers)
         ("C-c s-s" . mjs/save-org-buffers)
         ("C-c s-u" . org-revert-all-org-buffers)
         ("<f9>" . mjs/clock-in)
         ("S-<f9>" . mjs/clock-out)
         ("M-<f9>" . mjs/choose-clock-in)
         ("C-<f9>" . org-clock-goto)
         ("s-<f9>" . mjs/morning-sam)
         ("S-s-<f9>" . mjs/punch-out)
         :map org-mode-map
         ("M-." . org-open-at-point))

  :init
  (setq org-directory (expand-file-name "~/Documents/GTD"))
  (progn (add-hook 'org-mode-hook 'turn-on-auto-fill)
         (add-hook 'org-mode-hook 'flyspell-mode)
         (add-hook 'org-insert-heading-hook
                   'mjs/insert-heading-inactive-timestamp 'append))

  :config
  (load "org-hack")

  (defun mjs/insert-heading-inactive-timestamp ()
    (save-excursion
      (org-return)
      (org-cycle)
      (org-insert-time-stamp nil t t nil nil nil)))

  (defun mjs/push-xref-marker (&optional args)
    (xref-push-marker-stack))
  (advice-add #'org-open-at-point :before #'mjs/push-xref-marker)

  (progn
    (fullframe org-agenda org-agenda-quit)

    (setq org-default-notes-file (mjs/expand-org-file "inbox")
          org-use-property-inheritance t
          org-log-done 'time
          org-log-redeadline 'note
          org-log-reschedule 'note
          org-log-refile 'time
          org-log-repeat 'time
          org-log-into-drawer t

          org-agenda-files
          (mapcar #'mjs/expand-org-file '("todo" "defmethod" "nycvotes" "inbox" "inbox-mobile" "journal"))

          org-startup-indented t
          org-startup-folded 'fold

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
  :ensure org
  :functions (org-agenda-quit)
  :config
  (progn
    (defvar mjs/skip-habits-and-scheduled-and-deadlines
      '(org-agenda-skip-if nil '(scheduled deadline)))

    (setq org-agenda-sorting-strategy
          '((agenda time-up timestamp-up todo-state-down category-keep)
            (todo todo-state-up alpha-up)
            (tags todo-state-up alpha-up)
            (search todo-state-up alpha-up))

          org-stuck-projects
          '("+PROJECT=\"TRUE\"+LEVEL=2" ("TODO" "WAIT") nil "")

          org-agenda-clockreport-parameter-plist
          '(:link t :maxlevel 4 :fileskip0 t :compact t)

          org-agenda-start-on-weekday 0
          org-agenda-compact-blocks t
          org-agenda-follow-mode t
          org-agenda-include-diary t)

    (dolist (face '(org-done org-agenda-done org-headline-done))
      (set-face-attribute face nil :strike-through t))

    (setq
     org-agenda-custom-commands
     '(("h" "home"
        ((agenda "" ((org-agenda-span 'day)))
         (tags "+REFILE&+LEVEL=1"
               ((org-agenda-overriding-header "Tasks to Refile")))
         (tags-todo "+@CALL/!-WAIT"
                    ((org-agenda-skip-function
                      mjs/skip-habits-and-scheduled-and-deadlines)
                     (org-agenda-overriding-header "@CALL")))
         (tags-todo "+@EMAIL|+@SLACK|+@DISCORD/!-WAIT"
                    ((org-agenda-skip-function
                      mjs/skip-habits-and-scheduled-and-deadlines)
                     (org-agenda-overriding-header "@EMAIL|@SLACK|@DISCORD")))
         (tags-todo "+@ERRAND/!-WAIT"
                    ((org-agenda-skip-function
                      mjs/skip-habits-and-scheduled-and-deadlines)
                     (org-agenda-overriding-header "@ERRAND")))
         (tags-todo "+@MAC/!-WAIT"
                    ((org-agenda-skip-function
                      mjs/skip-habits-and-scheduled-and-deadlines)
                     (org-agenda-overriding-header "@MAC")))
         (tags-todo "+@WEB/!-WAIT"
                    ((org-agenda-skip-function
                      mjs/skip-habits-and-scheduled-and-deadlines)
                     (org-agenda-overriding-header "@WEB")))
         (tags-todo "+@HOME/!-WAIT"
                    ((org-agenda-skip-function
                      mjs/skip-habits-and-scheduled-and-deadlines)
                     (org-agenda-overriding-header "@HOME")))
         (tags-todo "+@ANY/!-WAIT"
                    ((org-agenda-skip-function
                      mjs/skip-habits-and-scheduled-and-deadlines)
                     (org-agenda-overriding-header "@ANY")))
         (tags-todo "+@WENDY/!-WAIT"
                    ((org-agenda-skip-function
                      mjs/skip-habits-and-scheduled-and-deadlines)
                     (org-agenda-overriding-header "@WENDY")))
         (tags-todo  "-@CLIENT&+@WORK|+@WORKMAC/!-WAIT"
                    ((org-agenda-skip-function
                      mjs/skip-habits-and-scheduled-and-deadlines)
                     (org-agenda-overriding-header "@WORK")))
         (tags-todo "-@CLIENT/WAIT"
                    ((org-agenda-overriding-header "WAITING-FOR")))))
       ("k" "work"
        ((agenda "" ((org-agenda-span 'day)))
         (tags "+REFILE&+LEVEL=1"
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
       ("i" "inbox"
        ((tags "+REFILE&+LEVEL=1"
               ((org-agenda-overriding-header "Tasks to Refile")))))
       ("r" . "Review speciality agenda")
       ("rx" "lost tasks" tags-todo "-{^@}")
       ("rw" "waiting" tags-todo "/WAIT")
       ("rp" "projects" tags "+PROJECT=\"TRUE\"+LEVEL=2")
       ("rt" "next-actions"  tags-todo "!-WAIT"
        ((org-agenda-skip-function mjs/skip-habits-and-scheduled-and-deadlines)))))))

(use-package org-capture
  :ensure org
  :init
  (add-hook 'org-capture-mode-hook 'turn-on-auto-fill)

  :config
  (setq org-capture-templates
        `(("c" "Capture" entry (file "")
           "* %?\n%U"
           :empty-lines 1)
          ("j" "Journal" entry (file+olp+datetree "journal.org")
           "* %T %?\n\n"
           :empty-lines 1)
          ("k" "Tickler" entry (file+headline "todo.org" "Tickler")
           "* %?\nSCHEDULED: %^t\n%U\n%a"
           :empty-lines 1)
          ("r" "Email" entry (file "")
           "* TODO Process '%:subject' from '%:from'\n%U\n%a"
           :empty-lines 1 :immediate-finish t)
          ("u" "URL" entry (file "")
           "* URL: %(org-mac-link-safari-get-frontmost-url)\n %U"
           :empty-lines 1)
          ("i" "Interruption" entry (file "")
           "* TODO %?\n%U\n%a\n"
           :clock-in t :clock-keep t :empty-lines 1 :jump-to-captured t)
          ("m" "MEMEX" plain (file denote-last-path) #'denote-org-capture
           :no-save t :immediate-finish nil :kill-buffer t :jump-to-captured t))

        org-capture-templates-contexts
        '(("r" ((in-mode . "message-mode")
                (in-mode . "gnus-summary-mode")
                (in-mode . "gnus-article-mode"))))))

(use-package ox-latex
  :ensure org
  :config (push '("adnd" "\\documentclass{adnd}"
                  ("\\part{%s}" . "\\chapter*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
                org-latex-classes))

(use-package org-checklist
  :ensure org-contrib
  :after org
  :init (add-to-list 'org-modules 'org-checklist))

(use-package org-indent
  :ensure org
  :diminish (org-indent-mode))

(defvar mjs/default-task-id "DEFAULT-TASK")
(defvar mjs/keep-clock-running nil)

(use-package org-clock
  :ensure org
  :functions (org-clock-in org-clock-out)
  :init
  (add-hook 'org-clock-out-hook 'mjs/clock-out-maybe 'append)

  :config
  (progn
    (declare-function org-agenda-clock-in "org-agenda")

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
  :ensure org
  :config (setq org-duration-format `h:mm))

(use-package org-present
  :config
  (setq org-present-text-scale 2.5)
  (mapcar #'(lambda (fun-pair)
              (add-hook 'org-present-mode-hook (car fun-pair))
              (add-hook 'org-present-mode-quit-hook (cdr fun-pair)))
          '((org-present-big . org-present-small)
            (org-display-inline-images . org-remove-inline-images)
            (org-present-hide-cursor . org-present-show-cursor)
            (org-present-read-only . org-present-read-write))))

(use-package org-mac-link)

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

(defun mjs/rename-denote-file ()
  (interactive)
  (if denote-dired-mode
      (call-interactively #'denote-rename-file)
    (call-interactively #'denote-rename-file-using-front-matter)))

(use-package denote-orphans
  :ensure nil
  :commands denote-orphans-visit-orphan)

(use-package denote
  :bind (("C-c m ." . mjs/visit-denote-directory)
         ("C-c m f" . denote-open-or-create)
         ("C-c m i" . #'denote-link)
         ("C-c m I" . #'denote-link-after-creating)
         ("C-c m n" . denote-create-note)
         ("C-c m b" . denote-link-backlinks)
         ("C-c m r" . mjs/rename-denote-file)
         ("C-c m o" . denote-orphans-visit-orphan))
  :hook ((dired-mode . denote-dired-mode-in-directories)
         (denote-dired-mode . dired-hide-details-mode))
  :config
  (progn
    (setq denote-backlinks-show-context t
          denote-directory (expand-file-name "memex" org-directory)
          denote-dired-directories (list denote-directory)
          denote-date-prompt-use-org-read-date t
          denote-known-keywords '("cite" "emacs" "lisp" "dnd")
          denote-infer-keywords t)
    (denote-rename-buffer-mode 1))

  (defun mjs/visit-denote-directory ()
    "Open dired buffer in `DENOTE-DIRECTORY`"
    (interactive)
    (dired denote-directory)))
