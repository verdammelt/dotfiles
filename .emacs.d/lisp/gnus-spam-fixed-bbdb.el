(with-eval-after-load 'bbdb-com
  (defun spam-clear-cache-BBDB (&rest immaterial)
    (spam-clear-cache 'spam-use-BBDB))

  (add-hook 'bbdb-change-hook 'spam-clear-cache-BBDB)

  ;; fixed version to use correct bbdb-create-internal for BBDB v3
  (defun spam-enter-ham-BBDB (addresses &optional remove)
    "Enter an address into the BBDB; implies ham (non-spam) sender"
    (dolist (from addresses)
      (when (stringp from)
        (let* ((parsed-address (gnus-extract-address-components from))
               (name (or (nth 0 parsed-address) "Ham Sender"))
               (remove-function (if remove
                                    'bbdb-delete-record-internal
                                  'ignore))
               (net-address (nth 1 parsed-address))
               (record (and net-address
                            (spam-exists-in-BBDB-p net-address))))
          (when net-address
            (gnus-message 6 "%s address %s %s BBDB"
                          (if remove "Deleting" "Adding")
                          from
                          (if remove "from" "to"))
            (if record
                (funcall remove-function record)
              (bbdb-create-internal name
                                    nil ; affix
                                    nil ; aka
                                    nil ; organization
                                    net-address ; mail
                                    nil ; phone
                                    nil ;address
                                    '((notes . "ham sender added by spam.el")) ; xfields
                                    )))))))

  (defun spam-BBDB-register-routine (articles &optional unregister)
    (let (addresses)
      (dolist (article articles)
        (when (stringp (spam-fetch-field-from-fast article))
          (push (spam-fetch-field-from-fast article) addresses)))
      ;; now do the register/unregister action
      (spam-enter-ham-BBDB addresses unregister)))

  (defun spam-BBDB-unregister-routine (articles)
    (spam-BBDB-register-routine articles t))

  (defsubst spam-exists-in-BBDB-p (net)
    (when (and (stringp net) (not (zerop (length net))))
      (bbdb-records)
      (bbdb-gethash (downcase net))))

  (defun spam-check-BBDB ()
    "Mail from people in the BBDB is classified as ham or non-spam"
    (let ((net (message-fetch-field "from")))
      (when net
        (setq net (nth 1 (gnus-extract-address-components net)))
        (if (spam-exists-in-BBDB-p net)
            t
          (if spam-use-BBDB-exclusive
              spam-split-group
            nil)))))

  ;; (defadvice spam-check-BBDB (around spam-check-BBDB-logged)
  ;;   (let ((return-value ad-do-it))
  ;;     (when (not return-value)
  ;; 	(message "spam-check-BBDB: '%s' was not found in the BBDB"))))
)
