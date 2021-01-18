;;;;
;;;; Mail setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(declare-function if-work "init")
(declare-function mjs/bbdb-init "init-bbdb")

(setq user-mail-address (if-work "mark@defmethod.io" "verdammelt@gmail.com"))

;; this way i have Gcc: etc. in my mail buffer
(setq mail-user-agent 'gnus-user-agent)

(use-package message
  :ensure nil
  :config
  (progn
    (mjs/bbdb-init 'message)

    ;; extra stuff for message buffers - spelling etc.
    (add-hook 'message-setup-hook 'bbdb-mail-aliases)
    (add-hook 'message-setup-hook 'footnote-mode)
    (add-hook 'message-setup-hook 'message-sort-headers)
    (add-hook 'message-setup-hook 'mml-secure-message-sign)
    (add-hook 'message-setup-hook 'sm-add-random-header)
    (add-hook 'message-setup-hook 'turn-on-flyspell)
    (add-hook 'message-send-hook 'ispell-message)

    (setq message-citation-line-function 'message-insert-formatted-citation-line
          message-citation-line-format "On %Y-%m-%dT%R%z, %f wrote:"
          message-wide-reply-confirm-recipients t
          message-default-headers "X-Attribution: MJS"
          message-sendmail-envelope-from 'header)))

(use-package mm-decode
  :ensure nil
  :config
  (progn
    (setq mm-default-directory (expand-file-name "~/Downloads")
          mm-verify-option 'always
          mm-decrypt-option 'ask
          mm-sign-option nil
          mm-encrypt-option 'guided
          mml-secure-openpgp-encrypt-to-self t
          mml-secure-openpgp-sign-with-sender t)
    (add-to-list 'mm-file-name-rewrite-functions
                 'mm-file-name-replace-whitespace)))

(use-package silly-mail
  :ensure nil
  :commands (sm-add-random-header)
  :defines (sm-mail-header-table)
  :config
  (progn
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
           sm-add-zippy-quote))))

(use-package sendmail
  :ensure nil
  :config
  (progn
    (setq send-mail-function 'sendmail-send-it
          mail-specify-envelope-from t
          mail-envelope-from 'header)))
