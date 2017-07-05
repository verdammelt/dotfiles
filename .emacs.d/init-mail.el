;;;;
;;;; Mail setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(setq user-mail-address (if-work "msimpson@defmethod.io" "verdammelt@gmail.com"))

;; this way i have Gcc: etc. in my mail buffer
(setq mail-user-agent 'gnus-user-agent)

;; getting bbdb in my message setup
(with-eval-after-load 'message
  (mjs/bbdb-init 'message)

  (autoload 'sm-add-random-header "silly-mail" nil t)

  (add-hook 'message-setup-hook 'bbdb-mail-aliases)

  (setq message-citation-line-function 'message-insert-formatted-citation-line)

  ;; extra stuff for message buffers - spelling etc.
  (add-hook 'message-setup-hook 'message-sort-headers)
  (add-hook 'message-setup-hook 'footnote-mode)
  (add-hook 'message-setup-hook 'turn-on-flyspell)
  (add-hook 'message-setup-hook 'sm-add-random-header)
  (add-hook 'message-send-hook 'ispell-message)
  (add-hook 'message-setup-hook 'mml-secure-message-sign)

  (setq message-default-headers "X-Attribution: MJS")

  (setq message-sendmail-envelope-from 'header))

(with-eval-after-load 'mm-decode
  (setq mm-default-directory (expand-file-name "~/Downloads"))
  (setq mm-verify-option 'always
        mm-decrypt-option 'ask
        mm-sign-option nil
        mm-encrypt-option 'guided
        mml-secure-openpgp-encrypt-to-self t
        mml-secure-openpgp-sign-with-sender t)
  (add-to-list 'mm-file-name-rewrite-functions 'mm-file-name-replace-whitespace))

(with-eval-after-load 'silly-mail
  (setq sm-mail-header-table
        '(sm-add-emacs-name
          sm-add-emacs-taunt
          (sm-add-horoscope           horoscope           "horoscope")
          (sm-add-kibology            kibologize          "kibologize")
          sm-add-meat
          (sm-add-shopping-list       shop-string         "shop")
          sm-add-tom-swifty
          sm-add-tomato
          (sm-add-uboat-death-message uboat-death-message "uboat")
          sm-add-zippy-quote)))

(with-eval-after-load 'sendmail
  (setq send-mail-function 'sendmail-send-it)
  (setq mail-specify-envelope-from t
        mail-envelope-from 'header))
