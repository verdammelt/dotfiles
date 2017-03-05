;;;; .emacs.el
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;
;;; First Things First
;;;
(setq message-log-max 10000)
(require 'cl-lib)

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
(defun mjs/load-init-file (file)
  (load (locate-user-emacs-file file)))

;;;
;;; Package
;;;
(package-initialize)
(mjs/load-init-file "init-package")

;;;
;;; Exterinalized config of specific things
;;;
(mjs/load-init-file "init-bbdb")
(mjs/load-init-file "init-display")
(mjs/load-init-file "init-gnus")
(mjs/load-init-file "init-ido")
(mjs/load-init-file "init-irc")
(mjs/load-init-file "init-javascript")
(mjs/load-init-file "init-latex")
(mjs/load-init-file "init-lisp")
(mjs/load-init-file "init-mail")
(mjs/load-init-file "init-misc")
(mjs/load-init-file "init-org")
(mjs/load-init-file "init-prog-elixir")

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
(projectile-mode)
(save-place-mode)
(savehist-mode)
(server-start)
(show-paren-mode)
(slime-setup '(slime-fancy slime-company))
(venv-initialize-interactive-shells)
(wrap-region-global-mode)
(yas-global-mode)
