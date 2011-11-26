;;;; .emacs.el
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Modified Time-stamp: <2011-10-12 23:18:01 mark>
;;;;
;; either of these can be useful for debugging an init time problem
;(toggle-debug-on-error)
(setq message-log-max 10000)

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(default ((t (:inherit nil :stipple nil :background "#002b36" :foreground "#839496" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 180 :width normal :foundry "apple" :family "Monaco")))))

;; can't live without my common lisp extensions!
(require 'cl)

(setq user-mail-address "damned@theworld.com")

;;;
;;; Load Path setup
;;;
(let ((default-directory "/opt/local/share/emacs/site-lisp"))
  (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(add-to-list 'load-path (locate-user-emacs-file "lisp/emacs-w3m"))
(add-to-list 'load-path (locate-user-emacs-file "elpa"))

(setenv "PATH" (concat "/opt/local/bin:" (getenv "PATH")))
(add-to-list 'exec-path (expand-file-name "/opt/local/bin"))

(require 'info)
(add-to-list 'Info-directory-list "/opt/local/share/info")
(add-to-list 'Info-directory-list "/opt/local/share/info/emacs")

(require 'package)
(add-to-list 'package-archives '("technomancy" .
                                 "http://repo.technomancy.us/emacs/") t)
(add-to-list 'package-archives '("marmalade" . 
                                 "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(package-initialize)

(require 'init-autoloads)
(require 'init-generic)

(require 'gnus)
(eval-after-load 'gnus '(require 'init-mail))
(eval-after-load 'bbdb '(require 'init-bbdb))
(eval-after-load 'bbdb-expire '(require 'init-bbdb))
(require 'init-lisp)
(require 'init-latex)
(eval-after-load 'calendar '(require 'init-calendar))
(require 'init-startup)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

