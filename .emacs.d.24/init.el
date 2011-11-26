;;;; .emacs.el
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Modified Time-stamp: <2011-10-12 23:18:01 mark>
;;;;
(require 'cl)				; I can't live without common lisp extensions!

;; either of these can be useful for debugging an init time problem
;(toggle-debug-on-error)
(setq message-log-max 10000)

;; I want to keep the customize stuff out of this config file.  I don't use
;; customize for much so I don't generally want to see it.
(setq custom-file (locate-user-emacs-file "init-custom.el"))
(load-file custom-file)

(setq user-mail-address "damned@theworld.com") ; default would be wrong from my laptop

;; set up package stuff including some extra repositories
(require 'package)
(mapcar #'(lambda (item) (add-to-list 'package-archives item :append))
	'(("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; set up load path
;; TODO: should i look to remove /opt/local/share/emacs/site-lisp?
(mapcar #'(lambda (path) (add-to-list 'load-path path))
	(list user-emacs-directory 
	      (locate-user-emacs-file "lisp")
	      (locate-user-emacs-file "lisp/emacs-w3m")
	      "/opt/local/share/emacs/site-lisp/bbdb/"))

;; bbdb stuff
(require 'bbdb-autoloads)
(autoload 'bbdb-expire-bbdb "bbdb-expire")
(eval-after-load 'bbdb '(require 'init-bbdb))

;; set up mail
(setq gnus-init-file (locate-user-emacs-file "init-gnus.el"))

;; I like silly headers
(autoload 'sm-add-random-header "silly-mail" nil t)
(add-hook 'message-setup-hook 'sm-add-random-header)

;; this way i have Gcc: etc. in my mail buffer
(autoload 'gnus-agent-possibly-save-gcc "gnus-agent")
(setq mail-user-agent 'gnus-user-agent)

;; getting bbdb in my message setup
(add-hook 'message-setup-hook 'bbdb-insinuate-message)
(add-hook 'message-setup-hook 'bbdb-define-all-aliases)
(add-hook 'message-setup-hook 'bbdb-message-mode-keys)

;; i want to read mail via gnus - duh!
(setq read-mail-command 'gnus)

;; add in my requested attribution
(setq message-default-headers "X-Attribution: MJS")


(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1))
(server-start)
(show-paren-mode)

(setq save-place t)
(require 'saveplace)
(setq save-place-file (locate-user-emacs-file ".places"))

(setq savehist-file (locate-user-emacs-file ".history"))
(savehist-mode)

(setq version-control t
      backup-directory-alist 
      (acons "." (locate-user-emacs-file ".backups") nil))
      
