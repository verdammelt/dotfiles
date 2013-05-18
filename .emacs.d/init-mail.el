;;;;
;;;; Mail setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Modified Time-stamp: <2013-05-18 11:47:19 mark>
;;;;
(setq user-mail-address "damned@theworld.com") ; default would be wrong from my laptop

;; this way i have Gcc: etc. in my mail buffer
(setq mail-user-agent 'gnus-user-agent)

;; getting bbdb in my message setup
(after 'message 
  (after 'bbdb
    (add-hook 'message-setup-hook 'bbdb-insinuate-message)
    (add-hook 'message-setup-hook 'bbdb-define-all-aliases)
    (defun bbdb-message-mode-keys ()
      (define-key message-mode-map (kbd "M-TAB") 'bbdb-complete-name))
    (add-hook 'message-setup-hook 'bbdb-message-mode-keys))
  
  ;; extra stuff for message buffers - spelling etc.
  (add-hook 'message-setup-hook 'footnote-mode)
  (add-hook 'message-setup-hook 'turn-on-flyspell)
  (add-hook 'message-send-hook 'ispell-message)

  (setq message-default-headers "X-Attribution: MJS")

  (after 'supercite
    (setq sc-preferred-header-style 1)))

(setq send-mail-function 'sendmail-send-it)

(provide 'init-mail)
