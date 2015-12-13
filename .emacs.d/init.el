;;;; .emacs.el
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;
;;; First Things First
;;;
(require 'cl-lib)

;; make sure the display is clean to start with
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message "damned")

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
(add-to-list 'exec-path "/usr/texbin" :append)
(setenv "PATH" (mapconcat 'identity exec-path ":"))

;;;
;;; Load Path
;;;
(add-to-list 'load-path (locate-user-emacs-file "lisp"))
(defun load-init-file (file)
  (load (locate-user-emacs-file file)))

;;;
;;; Package
;;;
(load-init-file "init-package")

;;;
;;; Exterinalized config of specific things
;;;
(load-init-file "init-bbdb")
(load-init-file "init-mail")
(load-init-file "init-gnus")
(load-init-file "init-latex")
(load-init-file "init-ido")
(load-init-file "init-org")
(load-init-file "init-lisp")
(load-init-file "init-misc")

;; set up the fonts / themes
(set-face-attribute 'default nil
                    :height 150
                    :family "DejaVu Sans Mono"
                    )
;; file:/usr/local/Cellar/emacs/24.3/share/emacs/24.3/etc/themes/tango-theme.el
(load-theme 'tango-dark)
(set-face-attribute 'highlight nil :background "#d3d7cf")
(set-face-attribute 'cursor nil :background "red")

;;;
;;; turn on all things that need turning on.
;;;
(display-battery-mode)
(display-time)
(server-start)
(edit-server-start)
(show-paren-mode)
(global-hl-line-mode)
(global-auto-revert-mode)
(savehist-mode)

;; setup midnight hooks
(require 'midnight)
(timer-activate midnight-timer)

(miniedit-install)

(projectile-global-mode)

(global-auto-complete-mode)

(wrap-region-global-mode)

(yas-global-mode)

(global-prettify-symbols-mode)

(browse-kill-ring-default-keybindings)

(venv-initialize-interactive-shells)

(global-flycheck-mode)
