;;;
;;; Mail setup
;;;
;;; Modified Time-stamp: <2012-07-29 13:15:21 mark>
;;;
(setq user-mail-address "damned@theworld.com") ; default would be wrong from my laptop
(setq gnus-init-file (locate-user-emacs-file "init-gnus.el"))

;; this way i have Gcc: etc. in my mail buffer
(setq mail-user-agent 'gnus-user-agent)

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

(setq send-mail-function 'sendmail-send-it)

;; keybinding for gnus
(defun switch-to-gnus () 
  (interactive) 
  (let ((group-buffer (get-buffer "*Group*")))
    (if group-buffer (switch-to-buffer group-buffer)
	(gnus))))
(global-set-key (kbd "<f6>") 'switch-to-gnus)

(setq sc-preferred-header-style 1)

(provide 'init-mail)
