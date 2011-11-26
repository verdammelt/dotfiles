;;;; Modified Time-stamp: <2011-03-05 13:06:28 mark>

;; I like silly headers
(autoload 'sm-add-random-header "silly-mail" nil t)
(add-hook 'message-setup-hook 'sm-add-random-header)

(autoload 'gnus-agent-possibly-save-gcc "gnus-agent")

(setq mail-user-agent 'gnus-user-agent)	;this way i have Gcc: etc.

;; (add-hook 'mail-citation-hook 'sc-cite-original)
;; (setq message-cite-function 'message-cite-original)
;; (eval-after-load 'sc '(bbdb-insinuate-sc))

;; (add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)
(add-hook 'message-setup-hook 'bbdb-insinuate-message)
;; (add-hook 'message-setup-hook 'footnote-mode)
(add-hook 'message-setup-hook 'bbdb-define-all-aliases)
(add-hook 'message-setup-hook 'bbdb-message-mode-keys)
;; (add-hook 'message-setup-hook 'turn-on-flyspell)
;; (add-hook 'message-setup-hook 'turn-on-font-lock)

;; (add-hook 'message-send-hook 'ispell-message)

;; (setq message-send-mail-function 'message-send-mail-with-sendmail)
;; (setq send-mail-function 'message-send-mail-with-sendmail)

;; i want to read mail via gnus - duh!
(setq read-mail-command 'gnus)

;; (setq sc-preferred-header-style 1)
;; (setq sc-preferred-attribution-list
;;       '("sc-lastchoice" "x-attribution" "sc-consult"
;;         "lastname" "firstname" "initials"))

;; check sc-attrib-selection-list
;; (setq sc-attrib-selection-list
;;       '(("sc-from-address"
;; 	 ((".*" . (bbdb/sc-consult-attr
;; 		   (sc-mail-field "sc-from-address")))))))

;; set sc-mail-glom-frame
;; (setq sc-mail-glom-frame
;;       '((begin                        (setq sc-mail-headers-start (point)))
;; 	("^x-attribution:[ \t]+.*$"   (sc-mail-fetch-field t) nil t)
;; 	("^\\S +:.*$"                 (sc-mail-fetch-field) nil t)
;; 	("^$"                         (progn (bbdb/sc-default)
;; 					     (list 'abort '(step . 0))))
;; 	("^[ \t]+"                    (sc-mail-append-field))
;; 	(sc-mail-warn-if-non-rfc822-p (sc-mail-error-in-mail-field))
;; 	(end                          (setq sc-mail-headers-end (point)))))

;; add in my requested attribution
(setq message-default-headers "X-Attribution: MJS")

;; (gnus-read-init-file)

;; (provide 'init-mail)
