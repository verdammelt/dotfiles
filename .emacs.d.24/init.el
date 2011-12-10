;;;; .emacs.el
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Modified Time-stamp: <2011-12-09 22:14:39 mark>
;;;;
(require 'cl)		; I can't live without common lisp extensions!
(setq message-log-max 10000)		; nice to see lots of messages

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
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") :append)
(package-initialize)

;;;
;;; Load Path
;;;
(mapcar #'(lambda (path) (add-to-list 'load-path path))
	(list user-emacs-directory 
	      (locate-user-emacs-file "lisp")
	      (locate-user-emacs-file "lisp/emacs-w3m")
	      (locate-user-emacs-file "lisp/bbdb")))

(require 'init-bbdb)
(require 'init-mail)
(require 'init-latex)

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
      trash-directory (expand-file-name "~/.Trash")) ; where to put the trash

;; time display the way i like it
(setq display-time-24hr-format t 
      display-time-day-and-date t)
(display-time)

;; making sure all buffers are named uniquely
(setq uniquify-after-kill-buffer-p t
      uniquify-buffer-name-style 'post-forward-angle-brackets)

(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1))

(server-start)

(show-paren-mode)

(ffap-bindings)

;; Update timestamps in file on save
(add-hook 'before-save-hook 'time-stamp)

;; TODO: something wrong here - no color - no duplex
;; ps-printing via a shell script for ps2pdf
(setq 
;; ps-lpr-command (expand-file-name "~/bin/psprint")
 ps-spool-duplex t
 )

;; Auto-mode-alist additions
(add-to-list 'auto-mode-alist '("*.md$" . markdown-mode))

