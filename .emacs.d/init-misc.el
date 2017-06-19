;;;;
;;;; MISC
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;; Save my place in files
(use-package saveplace
  :config (setq save-place-file (locate-user-emacs-file ".places")))

;; Save minibuffer history
(with-eval-after-load 'savehist
  (setq savehist-file (locate-user-emacs-file ".history")))

;; Backup files
(with-eval-after-load 'files
  (setq version-control t
        delete-old-versions t
        backup-by-copying-when-linked t
        backup-directory-alist
        (cl-acons "." (locate-user-emacs-file ".backups") nil)
        delete-by-moving-to-trash t
        trash-directory (expand-file-name "~/.Trash")))

;; time display the way i like it
(with-eval-after-load 'time
  (setq display-time-24hr-format t
        display-time-day-and-date t
        display-time-use-mail-icon t
        display-time-mail-face 'cursor ; (only background color used)
        display-time-format "%Y-%m-%dT%R"))

(global-set-key (kbd "s-p") 'ps-print-buffer)
(global-set-key (kbd "s-P") 'ps-print-region)
(with-eval-after-load 'ps-print
  (setq
   ps-lpr-command (expand-file-name "~/bin/psprint")
   ps-spool-duplex t))

;; calendar
(setq holiday-hebrew-holidays nil
      holiday-islamic-holidays nil
      holiday-oriental-holidays nil
      holiday-bahai-holidays nil)
(setq holiday-other-holidays
      '((holiday-sexp '(if (zerop (% year 4))
                           (calendar-gregorian-from-absolute
                            (1+ (calendar-dayname-on-or-before
                                 1 (+ 6 (calendar-absolute-from-gregorian
                                         (list 11 1 year)))))))
                      "US Presidential Election")))
(with-eval-after-load 'calendar
  (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
  (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

  (setq
   calendar-latitude +40.72541
   calendar-longitude -73.70928
   calendar-location-name "Floral Park, NY"
   calendar-time-display-form '(24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")"))
   diary-file "~/.diary"
   calendar-mark-diary-entries-flag t
   calendar-mark-holidays-flag t
   calendar-view-diary-initially-flag t
   calendar-view-holidays-initially-flag t)
  (calendar-set-date-style 'iso))

;; Editing text
(add-hook 'text-mode-hook 'fci-mode)
(setq sentence-end-double-space nil)

(with-eval-after-load 'battery
  (setq battery-mode-line-format "[%b%p%% %t] "))

(with-eval-after-load 'fill-column-indicator
  (setq fci-rule-color "red"))

;; abbrevs
(setq-default abbrev-mode t)
(with-eval-after-load 'abbrev (diminish 'abbrev-mode))

;; flyspell
(add-hook 'text-mode-hook 'flyspell-mode)
(with-eval-after-load 'flyspell
  (diminish 'flyspell-mode)
  (setq flyspell-use-meta-tab nil)
  (setq flyspell-abbrev-p t))

;; yasnippet
(with-eval-after-load 'yasnippet
  (diminish 'yas-minor-mode)
  (setq yas-prompt-functions
        '(yas-ido-prompt yas-completing-prompt)))

(global-set-key (kbd "<f7>") 'magit-status)
(with-eval-after-load 'magit
  (fullframe magit-status magit-mode-quit-window)
  (magithub-feature-autoinject t))

(autoload 'magithub-feature-autoinject "magithub")
(with-eval-after-load 'magithub
  (magithub-toggle-issues)
  (magithub-toggle-pull-requests))

(with-eval-after-load 'simple

  (setq-default indent-tabs-mode nil)
  (setq whitespace-style '(face indentation empty trailing)
        whitespace-action '(auto-cleanup warn-if-read-only)))
(with-eval-after-load 'whitespace
  (diminish 'whitespace-mode))

;; markdown-mode
(with-eval-after-load 'markdown-mode
  (setq markdown-command "markdown | smartypants"))

(with-eval-after-load 'paragraphs
  (setq sentence-end-double-space nil))

(with-eval-after-load 'ns-win
  (setq ns-use-srgb-colorspace t
        mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-function-modifier 'hyper))

(with-eval-after-load 'smex
  (setq smex-save-file (locate-user-emacs-file ".smex-items")))

;; Update timestamps in file on save
(add-hook 'before-save-hook 'time-stamp)

;; Misc
(with-eval-after-load 'gnutls
  (setq gnutls-min-prime-bits 1024))
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun mjs/change-size (size)
  (interactive "nsize: ")
  (if (< size 100) (setq size (* 10 size)))
  (set-face-attribute 'default nil :height size))

(global-set-key (kbd "H-s") 'mjs/change-size)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)

(global-set-key (kbd "s-j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "s-J") 'avy-goto-char-2)
(avy-setup-default)

(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurance of CHAR.")
(autoload 'forward-to-word "misc"
  "Move forward until encountering the beginning of a word.
With argument, do this that many times.")
(autoload 'backward-to-word "misc"
  "Move backward until encountering the end of a word.
With argument, do this that many times.")
(global-set-key (kbd "M-F") 'forward-to-word)
(global-set-key (kbd "M-B") 'backward-to-word)
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;; wrap-region
(with-eval-after-load 'wrap-region
  (diminish 'wrap-region-mode)
  (wrap-region-add-wrapper "+" "+" nil 'org-mode)
  (wrap-region-add-wrapper "_" "_" nil 'markdown-mode)
  (wrap-region-add-wrapper "*" "*" nil 'markdown-mode))

(defun mjs/add-wgrep-key ()
  (define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))
(add-hook 'grep-mode-hook 'mjs/add-wgrep-key)

;; always want browser to open in background.
(defun mjs/browse-url-default-macosx-browser-background (url &optional _new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (start-process (concat "open " url) nil "open" "-g" url))
(setq browse-url-browser-function 'mjs/browse-url-default-macosx-browser-background)

(with-eval-after-load 'ediff
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(with-eval-after-load 'shell
  (add-to-list 'explicit-bash-args "--login"))

;; search for symbol at point (by Jorgen Schäfer)
(define-key isearch-mode-map (kbd "C-d") 'fc/isearch-yank-symbol)
(defun fc/isearch-yank-symbol ()
  "Yank the symbol at point into the isearch minibuffer.

C-w does something similar in isearch, but it only looks for
the rest of the word. I want to look for the whole string. And
symbol, not word, as I need this for programming the most."
  (interactive)
  (isearch-yank-string
   (save-excursion
     (when (and (not isearch-forward) isearch-other-end)
       (goto-char isearch-other-end))
     (thing-at-point 'symbol))))

(setq confirm-kill-emacs 'yes-or-no-p )

(add-hook 'after-save-hook
          'executable-make-buffer-file-executable-if-script-p)

(define-key global-map [remap list-buffers] 'ibuffer)

(setq scroll-preserve-screen-position t)

;; Setup Hyperspec info file
(add-to-list 'Info-directory-list (expand-file-name "~/.emacs.d/info"))

(require 'info-look)
(info-lookup-add-help
 :mode 'lisp-mode
 :regexp "[^][()'\" \t\n]+"
 :ignore-case t
 :doc-spec '(("(ansicl)Symbol Index" nil nil nil)))

(add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
(add-hook 'edit-server-done-hook 'edit-server-maybe-htmlize-buffer)
(add-hook 'edit-server-done-hook (lambda () (kill-ring-save (point-min) (point-max))))

(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(with-eval-after-load 'keyfreq
  (defvar keyfreq-file)
  (defvar keyfreq-file-lock)
  (setq keyfreq-file (locate-user-emacs-file ".keyfreq")
        keyfreq-file-lock (concat keyfreq-file ".lock")))

(with-eval-after-load 'calc
  (setq math-additional-units
        '((fort "14 day" "Fortnight")
          (bit nil "Bit")
          (byte "8 bit" "Byte"))
        math-units-table nil))

(with-eval-after-load 'flycheck
  (setq flycheck-completing-read-function 'ido-completing-read)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (flycheck-credo-setup))

;;;;
;;;; ========== experimental calendar loading ==========
;;;;

(advice-add 'icalendar--convert-recurring-to-diary :filter-return #'string-trim)

(defun mjs/eradicate-file (file)
  (let ((buffer (find-buffer-visiting file))
        (file-exists-p (file-exists-p file)))
    (and buffer (kill-buffer buffer))
    (and file-exists-p (delete-file file))))

(defun mjs/import-calendars (calendars)
  (let ((current-buffer (current-buffer)))
    (let ((diary-import-file "~/.diary.imported"))
      (mjs/eradicate-file diary-import-file)
      (mapc (lambda (cal)
              (let ((tmpfile (url-file-local-copy cal)))
                (icalendar-import-file tmpfile diary-import-file)
                (mjs/eradicate-file tmpfile)))
            calendars))
    (switch-to-buffer current-buffer)))

(defun mjs/import-all-calendars ()
  (defvar google-calendars)
  (load "~/.diary.calendars-to-import.el")
  (mjs/import-calendars google-calendars))

;; turning off for now - re-evaluate [2017-04-30]
;; (defvar mjs/calendar-import-timer
;;   (run-with-idle-timer (* 60 10) t #'mjs/import-all-calendars))

(use-package browse-kill-ring
  :defer 2
  :config (browse-kill-ring-default-keybindings))
