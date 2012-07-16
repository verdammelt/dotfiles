(require 'gnus-mlspl)

;; modified version of the function from gnus-mlspl.el which only got info from group parameters in the newsrc file
;; this version gets from newsrc and gnus-parameters variable correctly.
(defun gnus-group-split-fancy
  (&optional groups no-crosspost catch-all)
  "Uses information from group parameters in order to split mail.
It can be embedded into `nnmail-split-fancy' lists with the SPLIT

\(: gnus-group-split-fancy GROUPS NO-CROSSPOST CATCH-ALL\)

GROUPS may be a regular expression or a list of group names, that will
be used to select candidate groups.  If it is omitted or nil, all
existing groups are considered.

if NO-CROSSPOST is omitted or nil, a & split will be returned,
otherwise, a | split, that does not allow crossposting, will be
returned.

For each selected group, a SPLIT is composed like this: if SPLIT-SPEC
is specified, this split is returned as-is (unless it is nil: in this
case, the group is ignored).  Otherwise, if TO-ADDRESS, TO-LIST and/or
EXTRA-ALIASES are specified, a regexp that matches any of them is
constructed (extra-aliases may be a list).  Additionally, if
SPLIT-REGEXP is specified, the regexp will be extended so that it
matches this regexp too, and if SPLIT-EXCLUDE is specified, RESTRICT
clauses will be generated.

If CATCH-ALL is nil, no catch-all handling is performed, regardless of
catch-all marks in group parameters.  Otherwise, if there is no
selected group whose SPLIT-REGEXP matches the empty string, nor is
there a selected group whose SPLIT-SPEC is 'catch-all, this fancy
split (say, a group name) will be appended to the returned SPLIT list,
as the last element of a '| SPLIT.

For example, given the following group parameters:

nnml:mail.bar:
\((to-address . \"bar@femail.com\")
 (split-regexp . \".*@femail\\\\.com\"))
nnml:mail.foo:
\((to-list . \"foo@nowhere.gov\")
 (extra-aliases \"foo@localhost\" \"foo-redist@home\")
 (split-exclude \"bugs-foo\" \"rambling-foo\")
 (admin-address . \"foo-request@nowhere.gov\"))
nnml:mail.others:
\((split-spec . catch-all))

Calling (gnus-group-split-fancy nil nil \"mail.others\") returns:

\(| (& (any \"\\\\(bar@femail\\\\.com\\\\|.*@femail\\\\.com\\\\)\"
	   \"mail.bar\")
      (any \"\\\\(foo@nowhere\\\\.gov\\\\|foo@localhost\\\\|foo-redist@home\\\\)\"
	   - \"bugs-foo\" - \"rambling-foo\" \"mail.foo\"))
   \"mail.others\")"
  (let* ((newsrc (cdr gnus-newsrc-alist))
	 split)
    (dolist (info newsrc)
      (let* ((group (gnus-info-group info))
	    (params (gnus-group-find-parameter group)))
	;; For all GROUPs that match the specified GROUPS
	(when (or (not groups)
		  (and (listp groups)
		       (memq group groups))
		  (and (stringp groups)
		       (string-match groups group)))
	  (let ((split-spec (assoc 'split-spec params)) group-clean)
	    ;; Remove backend from group name
	    (setq group-clean (string-match ":" group))
	    (setq group-clean
		  (if group-clean
		      (substring group (1+ group-clean))
		    group))
	    (if split-spec
		(when (setq split-spec (cdr split-spec))
		  (if (eq split-spec 'catch-all)
		      ;; Emit catch-all only when requested
		      (when catch-all
			(setq catch-all group-clean))
		    ;; Append split-spec to the main split
		    (push split-spec split)))
	      ;; Let's deduce split-spec from other params
	      (let ((to-address (cdr (assoc 'to-address params)))
		    (to-list (cdr (assoc 'to-list params)))
		    (extra-aliases (cdr (assoc 'extra-aliases params)))
		    (split-regexp (cdr (assoc 'split-regexp params)))
		    (split-exclude (cdr (assoc 'split-exclude params))))
		(when (or to-address to-list extra-aliases split-regexp)
		  ;; regexp-quote to-address, to-list and extra-aliases
		  ;; and add them all to split-regexp
		  (setq split-regexp
			(concat
			 "\\("
			 (mapconcat
			  'identity
			  (append
			   (and to-address (list (regexp-quote to-address)))
			   (and to-list (list (regexp-quote to-list)))
			   (and extra-aliases
				(if (listp extra-aliases)
				    (mapcar 'regexp-quote extra-aliases)
				  (list extra-aliases)))
			   (and split-regexp (list split-regexp)))
			  "\\|")
			 "\\)"))
		  ;; Now create the new SPLIT
		  (push (append
			 (list 'any split-regexp)
			 ;; Generate RESTRICTs for SPLIT-EXCLUDEs.
			 (if (listp split-exclude)
			     (apply #'append
				    (mapcar (lambda (arg) (list '- arg))
					    split-exclude))
			   (list '- split-exclude))
			 (list group-clean))
			split)
		  ;; If it matches the empty string, it is a catch-all
		  (when (string-match split-regexp "")
		    (setq catch-all nil)))))))))
    ;; Add catch-all if not crossposting
    (if (and catch-all no-crosspost)
	(push catch-all split))
    ;; Move it to the tail, while arranging that SPLITs appear in the
    ;; same order as groups.
    (setq split (reverse split))
    ;; Decide whether to accept cross-postings or not.
    (push (if no-crosspost '| '&) split)
    ;; Even if we can cross-post, catch-all should not get
    ;; cross-posts.
    (if (and catch-all (not no-crosspost))
	(setq split (list '| split catch-all)))
    split))

(provide 'gnus-group-split-fancy)