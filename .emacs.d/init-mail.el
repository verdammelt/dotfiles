;;;;
;;;; Mail setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Modified Time-stamp: <2013-08-15 21:46:38 mark>
;;;;
(setq user-mail-address "damned@theworld.com") ; default would be wrong from my laptop

;; this way i have Gcc: etc. in my mail buffer
(setq mail-user-agent 'gnus-user-agent)

;; getting bbdb in my message setup
(after 'message 
  
  
  ;; extra stuff for message buffers - spelling etc.
  (add-hook 'message-setup-hook 'footnote-mode)
  (add-hook 'message-setup-hook 'turn-on-flyspell)
  (add-hook 'message-send-hook 'ispell-message)

  (setq message-default-headers "X-Attribution: MJS")
  
  (setq message-sendmail-envelope-from 'header)

  (after 'supercite
    (setq sc-preferred-header-style 1)))

(after 'sendmail 
  (setq send-mail-function 'sendmail-send-it)
  (setq mail-specify-envelope-from t
	mail-envelope-from 'header))

(provide 'init-mail)
