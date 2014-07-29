;;;; .emacs.el
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Modified Time-stamp: <2014-07-29 20:24:16 mark>
;;;;

;;; 
;;; First Things First
;;;
(require 'cl)		

;; make sure the display is clean to start with
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message "damned")

;; cute little wrapper around eval-after-load
(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

;;;
;;; Customize
;;;
;; I want to keep the customize stuff out of this config file.  I
;; don't use customize for much so I don't generally want to see it.
(setq custom-file (locate-user-emacs-file "init-custom.el"))
(load-file custom-file)

;;;
;;; Exec Path
;;;
(add-to-list 'exec-path "/usr/local/share/npm/bin")
(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path (expand-file-name "~/Bin"))
(add-to-list 'exec-path "/usr/texbin" :append)
(setenv "PATH" (mapconcat 'identity exec-path ":"))

;;;
;;; Load Path
;;;
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path (locate-user-emacs-file "lisp"))

;;;
;;; Package
;;;
(require 'init-package)

;;;
;;; Exterinalized config of specific things
;;;
(require 'init-bbdb)
(require 'init-mail)
(require 'init-gnus)
(require 'init-latex)
(require 'init-ido)
(require 'init-org)
(require 'init-lisp)
(require 'init-misc)

;; set up the fonts / themes
(set-face-attribute 'default nil 
		    :height 140
		    :family "Source_Code_Pro")
;; file:/usr/local/Cellar/emacs/24.3/share/emacs/24.3/etc/themes/tango-theme.el
(load-theme 'tango)
(set-face-attribute 'highlight nil
		    :background (face-background 'mode-line))

;;;
;;; turn on all things that need turning on.
;;;
(require 'battery-patch)
(display-battery-mode)
(display-time)
(server-start)
(show-paren-mode)
(global-hl-line-mode)
(global-auto-revert-mode)
(savehist-mode)

;; ido mode
(ido-mode)
(ido-everywhere)
(ido-ubiquitous)
(require 'ido-hacks)
(ido-hacks-mode)
(flx-ido-mode 1)
(smex-initialize)			; must be after ido-hacks!

;; setup midnight hooks
(require 'midnight)
(timer-activate midnight-timer)

(require 'uniquify)

;; setup miniedit - multi-line editing of the mini-buffer
(require 'miniedit)
(miniedit-install)

(projectile-global-mode)

(global-auto-complete-mode t)

(wrap-region-global-mode t)

(yas-global-mode 1)


(global-pretty-mode)

(browse-kill-ring-default-keybindings)

