;;;; .emacs.el
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Modified Time-stamp: <2011-12-08 00:01:12 mark>
;;;;
(require 'cl)		; I can't live without common lisp extensions!

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
	      (locate-user-emacs-file "lisp/bbdb")))

;; bbdb stuff
(require 'bbdb-autoloads)
(autoload 'bbdb-expire-bbdb "bbdb-expire")
(eval-after-load 'bbdb 
  (progn (require 'bbdb-expire)
	 (setq bbdb-complete-name-allow-cycling t
	       bbdb/news-auto-create-p t
	       bbdb-expire-this-old-30)
	 
	 (bbdb-expire-field-foo-p 'mail-alias)
	 (add-to-list 'bbdb-expire-preservation-functions
		      #'bbdb-expire-field-mail-alias-p)
	 (add-hook 'bbdb-expire-pre-expire-hook
		   #'(lambda () (message "bbdb expiry start: %d records"
				(length (bbdb-records)))))
	 (add-hook 'bbdb-expire-post-expire-hook
		   #'(lambda () (message "bbdb-expiry finish: %d records"
					 (length (bbdb-records)))))
	 
	 (defun bbdb-message-mode-keys ()
	   (define-key message-mode-map (kbd "M-TAB") 'bbdb-complete-name))
	 
	 (require 'supercite)
	 (bbdb-insinuate-sc)
	 (require 'gnus)
	 (bbdb-initialize 'gnus)
	 (require 'message)
	 (bbdb-initialize 'messsage)
	 (bbdb-expire-initialize)))

;; setup midnight hooks
(require 'midnight)
(timer-activate midnight-timer)

;; setting up midnight-hook - do these things at midnight
(add-hook 'midnight-hook #'bbdb-expire-bbdb)
(add-hook 'midnight-hook #'clean-buffer-list)

;; set up mail
(setq gnus-init-file (locate-user-emacs-file "init-gnus.el"))

;; ;; I like silly headers
;; (autoload 'sm-add-random-header "silly-mail" nil t)
;; (add-hook 'message-setup-hook 'sm-add-random-header)

;; this way i have Gcc: etc. in my mail buffer
(autoload 'gnus-agent-possibly-save-gcc "gnus-agent")
(setq mail-user-agent 'gnus-user-agent)

;; ;; supercite setup
;; (add-hook 'mail-citation-hook 'sc-cite-original)
;; (setq message-cite-function 'message-cite-original)
;; (eval-after-load 'sc '(bbdb-insinuate-sc))


;; getting bbdb in my message setup
(add-hook 'message-setup-hook 'bbdb-insinuate-message)
(add-hook 'message-setup-hook 'bbdb-define-all-aliases)
(add-hook 'message-setup-hook 'bbdb-message-mode-keys)

;; extra stuff for message buffers - spelling etc.
(add-hook 'message-setup-hook 'footnote-mode)
(add-hook 'message-setup-hook 'turn-on-flyspell)

(add-hook 'message-send-hook 'ispell-message)


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
      delete-old-versions t
      backup-directory-alist 
      (acons "." (locate-user-emacs-file ".backups") nil))
      

;; keybinding for gnus
(defun switch-to-gnus () 
  (interactive) 
  (let ((group-buffer (get-buffer "*Group*")))
    (if group-buffer (switch-to-buffer group-buffer)
	(gnus))))
(global-set-key (kbd "<f6>") 'switch-to-gnus)

;; remove splash screen crap
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message "damned")


;; MacOSX sort of trash directory
(setq delete-by-moving-to-trash t
      trash-directory (expand-file-name "~/.Trash")) ; where to put the trash

;; time display the way i like it
(setq display-time-24hr-format t 
      display-time-day-and-date t)

;; making sure all buffers are named uniquely
(setq uniquify-after-kill-buffer-p t
      uniquify-buffer-name-style 'post-forward-angle-brackets)

(display-time)

(ffap-bindings)

;; set up tex/latex
(require 'tex-site)
(require 'tex)
(TeX-global-PDF-mode)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; TODO: something wrong here - no color - no duplex
;; ps-printing via a shell script for ps2pdf
(setq ps-lpr-command (expand-file-name "~/bin/psprint"))

(add-to-list 'auto-mode-alist '("*.md$" . markdown-mode))

(add-hook 'before-save-hook 'time-stamp)
