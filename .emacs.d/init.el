;;;; .emacs.el
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;
;;; First Things First
;;;
(setq message-log-max 10000)

(defvar *is-work-machine* (string-match-p "svadilfari" (system-name)))
(defmacro if-work (then else)
  `(if *is-work-machine* ,then ,else))

;; FIXME Problem with emacs 30 on MacOS Sequoia at the moment. Not getting PATH
;; from the Info.plist file. So here we do it manually
(if-work
 (progn
   (setq exec-path
         '(
           "/Users/mark/.nvm/versions/node/v20.2.0/bin"
           "/Users/mark/.rbenv/shims"
           "/Users/mark/Bin"
           "/opt/homebrew/bin"
           "/opt/homebrew/sbin"
           "/usr/local/bin"
           "/System/Cryptexes/App/usr/bin"
           "/usr/bin"
           "/bin"
           "/usr/sbin"
           "/sbin"
           "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/local/bin"
           "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/bin"
           "/var/run/com.apple.security.cryptexd/codex.system/bootstrap/usr/appleinternal/bin"
           "/Library/Apple/usr/bin"
           "/Library/TeX/texbin"
           "/opt/homebrew/Cellar/emacs-plus@30/30.0.93/libexec/emacs/30.0.93/aarch64-apple-darwin24.2.0"))
   (setenv "PATH" (mapconcat 'identity exec-path ":")))
 nil)

;;;
;;; Customize
;;;
;; I want to keep the customize stuff out of this config file.  I
;; don't use customize for much so I don't generally want to see it.
(setq custom-file (locate-user-emacs-file "init-custom.el"))
(load custom-file)

;;;
;;; Load Path
;;;
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(defun mjs/load-init-file (file &optional noerror)
  (load (locate-user-emacs-file file) noerror))

;;;
;;; Package
;;;
(mjs/load-init-file "init-package")

;;;
;;; Exterinalized config of specific things
;;;
(mjs/load-init-file "init-bbdb")
(mjs/load-init-file "init-calendar")
(mjs/load-init-file "init-display")
(mjs/load-init-file "init-gnus")
(mjs/load-init-file "init-latex")
(mjs/load-init-file "init-mail")
(mjs/load-init-file "init-misc")
(mjs/load-init-file "init-org")
(mjs/load-init-file "init-prog")
(mjs/load-init-file "init-prog-lisp")

(mjs/load-init-file "secrets" 'noerror)

(message "init file loading complete: %s" (emacs-init-time))
