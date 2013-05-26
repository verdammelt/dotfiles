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
