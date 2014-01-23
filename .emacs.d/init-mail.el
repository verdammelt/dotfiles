;;;;
;;;; Mail setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Modified Time-stamp: <2014-01-16 17:04:25 mjs>
;;;;
(setq user-mail-address "msimpson@cyrusinnovation.com") ; default would be wrong from my laptop

;; this way i have Gcc: etc. in my mail buffer
(setq mail-user-agent 'gnus-user-agent)

;; getting bbdb in my message setup
(after 'message 
  (autoload 'sm-add-random-header "silly-mail" nil t)

  ;; extra stuff for message buffers - spelling etc.
  (add-hook 'message-setup-hook 'footnote-mode)
  (add-hook 'message-setup-hook 'turn-on-flyspell)
  (add-hook 'message-setup-hook 'sm-add-random-header)
  (add-hook 'message-send-hook 'ispell-message)

  (setq message-default-headers "X-Attribution: MJS")
  
  (setq message-sendmail-envelope-from 'header)

  (after 'supercite
    (setq sc-preferred-header-style 1)))

(after 'silly-mail
  (setq sm-mail-header-table
	'(sm-add-emacs-name
	  sm-add-emacs-taunt
	  (sm-add-flame               flame               "flame")
	  (sm-add-horoscope           horoscope           "horoscope")
	  (sm-add-kibology            kibologize          "kibologize")
	  sm-add-meat
	  (sm-add-shopping-list       shop-string         "shop")
	  sm-add-tom-swifty
	  sm-add-tomato
	  (sm-add-uboat-death-message uboat-death-message "uboat")
	  sm-add-zippy-quote)))

(after 'sendmail 
  (setq send-mail-function 'sendmail-send-it)
  (add-hook 'mail-citation-hook 'sc-cite-original)
  (setq mail-specify-envelope-from t
	mail-envelope-from 'header))

(provide 'init-mail)
