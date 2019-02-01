;;;; .emacs.el
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;
;;; First Things First
;;;
(setq message-log-max 10000)
(require 'cl-lib)


(defvar *is-work-machine* (string-match-p "svadilfari" (system-name)))
(defmacro if-work (then else)
  `(if *is-work-machine* ,then ,else))

;;;
;;; Customize
;;;
;; I want to keep the customize stuff out of this config file.  I
;; don't use customize for much so I don't generally want to see it.
(setq custom-file (locate-user-emacs-file "init-custom.el"))
(load custom-file)

;;;
;;; Exec Path
;;;
(add-to-list 'exec-path "/usr/local/share/npm/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path (expand-file-name "~/Bin"))
(add-to-list 'exec-path "/Library/TeX/texbin" :append)

(defun mjs/set-path-envvar-from-exec-path ()
  (setenv "PATH" (mapconcat 'identity exec-path ":")))

(mjs/set-path-envvar-from-exec-path)

;;;
;;; Load Path
;;;
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

(defun mjs/load-init-file (file &optional noerror)
  (load (locate-user-emacs-file file) noerror))

;;;
;;; Package
;;;
(package-initialize)
(mjs/load-init-file "init-package")

;;;
;;; Exterinalized config of specific things
;;;
(mjs/load-init-file "init-bbdb")
(mjs/load-init-file "init-calendar")
(mjs/load-init-file "init-company")
(mjs/load-init-file "init-display")
(mjs/load-init-file "init-gnus")
(mjs/load-init-file "init-ido")
(mjs/load-init-file "init-latex")
(mjs/load-init-file "init-mail")
(mjs/load-init-file "init-misc")
(mjs/load-init-file "init-org")
(mjs/load-init-file "init-prog")
(mjs/load-init-file "init-prog-elixir")
(mjs/load-init-file "init-prog-go")
(mjs/load-init-file "init-prog-javascript")
(mjs/load-init-file "init-prog-lisp")
(mjs/load-init-file "init-prog-typescript")
(mjs/load-init-file "init-projectile")

(mjs/load-init-file "secrets" 'noerror)

(message "init file loading complete")
