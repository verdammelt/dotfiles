;;;; Modified Time-stamp: <2011-08-14 09:53:13 mark>
;;; start up features / modes / displays
(ffap-bindings)
(global-font-lock-mode 1)
(highlight-tail-reload)
(iswitchb-mode 1)
;(menu-bar-mode -1)
(when window-system
  (scroll-bar-mode -1)
  (tool-bar-mode -1))
(server-start)
(show-paren-mode 1)
(timer-activate midnight-timer)
(transient-mark-mode 1)
(setq desktop-dirname user-emacs-directory)
(desktop-save-mode 1)
(setq savehist-file (locate-user-emacs-file ".savehist"))
(savehist-mode 1)
(setq save-place-file (locate-user-emacs-file ".saveplace"))
(setq-default save-place t)
(require 'saveplace)

(if (locate-library "edit-server")
		(progn
			(require 'edit-server)
			(setq edit-server-new-frame nil)
			(edit-server-start)))

(add-hook 'ruby-mode-hook
          '(lambda ()
             (add-to-list 
              (make-local-variable 'paredit-space-for-delimiter-predicates)
              (lambda (_ _) nil))
             ;; (abbrev-mode 1)
             (ruby-electric-mode 1)
             (snippet-with-abbrev-table
              'rspec-mode-abbrev-table
              ("it" . "it \"$${what exactly?}\" do\n  $.\nend")))
          :append)

(provide 'init-startup)
