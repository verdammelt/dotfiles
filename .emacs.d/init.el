;;;; .emacs.el
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Modified Time-stamp: <2013-05-12 16:22:49 mark>
;;;;
(require 'cl)		; I can't live without common lisp extensions!
(setq message-log-max 10000)		; nice to see lots of messages

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "/Users/mark/Bin")
(add-to-list 'exec-path "/usr/texbin" :append)

(setenv "PATH" (mapconcat 'identity exec-path ":"))

;;;
;;; Customize
;;;
;; I want to keep the customize stuff out of this config file.  I
;; don't use customize for much so I don't generally want to see it.
(setq custom-file (locate-user-emacs-file "init-custom.el"))
(load-file custom-file)

;;;
;;; Package
;;;
(require 'package)
(add-to-list 'package-archives
	     '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives 
	     '("marmalade" . "http://marmalade-repo.org/packages/") 
	     :append)

(package-initialize)

;;;
;;; Load Path
;;;
(mapcar #'(lambda (path) (add-to-list 'load-path path))
	(list user-emacs-directory 
	      (locate-user-emacs-file "lisp")
	      ;; (locate-user-emacs-file "lisp/slime")
	      (locate-user-emacs-file "lisp/midje-mode-0.1.1")
	      (locate-user-emacs-file "lisp/bbdb")))

(require 'init-bbdb)
(require 'init-mail)
(require 'init-gnus)
(require 'init-latex)
(require 'init-ido)
(require 'init-org)

;;; 
;;; Misc Settings
;;;
;; setup midnight hooks
(require 'midnight)
(timer-activate midnight-timer)
(add-hook 'midnight-hook #'clean-buffer-list)

;; setup miniedit - multi-line editing of the mini-buffer
(require 'miniedit)
(miniedit-install)

;; Save my place in files
(setq save-place t)
(require 'saveplace)
(setq save-place-file (locate-user-emacs-file ".places"))

;; Save minibuffer history
(setq savehist-file (locate-user-emacs-file ".history"))
(savehist-mode)

;; Backup files
(setq version-control t
      delete-old-versions t
      backup-directory-alist 
      (acons "." (locate-user-emacs-file ".backups") nil))

;; remove splash screen crap
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message "damned")

;; MacOSX sort of trash directory
(setq delete-by-moving-to-trash t
      trash-directory (expand-file-name "~/.Trash"))

;; time display the way i like it
(setq display-time-24hr-format t 
      display-time-day-and-date t)
(display-time)

;; making sure all buffers are named uniquely
(setq uniquify-after-kill-buffer-p t
      uniquify-buffer-name-style 'post-forward-angle-brackets)

;; dislike the scrollback and toolbar
(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

;; start server for emacsclient
(server-start)

;; blink matching parens
(show-paren-mode)

;; highlight current line in all buffers
(global-hl-line-mode)

;; Update timestamps in file on save
(add-hook 'before-save-hook 'time-stamp)

(setq 
 ps-lpr-command (expand-file-name "~/bin/psprint")
 ps-spool-duplex t)

;; calendar
(setq 
 calendar-latitude +42.358056
 calendar-location-name "Cambridge, MA"
 calendar-longitude -71.113056
 calendar-time-display-form '(24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")"))
 diary-file "~/.diary"
 calendary-date-style 'european
 calendar-mark-diary-entries-flag t
 calendar-mark-holidays-flag t
 calendar-view-diary-initially-flag t
 calendar-view-holidays-initially-flag t)

;; Auto-mode-alist additions
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))

;; snippets
(setq yas-also-auto-indent-first-line t)
(yas-global-mode t)

;; Editing text
(setq fci-rule-color "red") 
(add-hook 'text-mode-hook 'turn-on-fci-mode)

(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'turn-on-fci-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'paredit-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook 'paredit-mode)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)

(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'nrepl-interaction-mode 'nrepl-turn-on-eldoc-mode)

(eval-after-load "nrepl"
  (setq nrepl-popup-stacktraces nil))


;; turning on eldoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(add-hook 'ruby-mode-hook 'flymake-ruby-load)

(global-set-key (kbd "<f7>") 'magit-status)

(global-set-key (kbd "s-z") 'ns-toggle-fullscreen)

(setq battery-mode-line-format "[%b%p%% %t]")

