;;;; .emacs.el
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;
;;; First Things First
;;;
(setq message-log-max 10000)
(package-initialize)
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
(add-to-list 'exec-path "/Library/TeX/texbin" :append)
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
(load-init-file "init-javascript")
(load-init-file "init-misc")

;;;
;;; set up the fonts / themes
;;;
;; Fallback font - helps with Unicode
(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "Symbola"))
(set-face-attribute 'default nil
                    :height 180
                    :family "DejaVu Sans Mono")
(load-theme 'tango-2)
(set-cursor-color "red")

;;;
;;; turn on all things that need turning on.
;;;
(browse-kill-ring-default-keybindings)
(display-battery-mode)
(display-time)
(edit-server-start)
(global-company-mode)
(company-quickhelp-mode)
(global-auto-revert-mode)
(global-flycheck-mode)
(global-git-commit-mode)
(global-hl-line-mode)
(global-prettify-symbols-mode)
(keyfreq-mode)
(midnight-mode)
(miniedit-install)
(mjs/turn-on-ido)
(projectile-global-mode)
(save-place-mode)
(savehist-mode)
(server-start)
(show-paren-mode)
(slime-setup '(slime-fancy slime-company))
(venv-initialize-interactive-shells)
(wrap-region-global-mode)
(yas-global-mode)
