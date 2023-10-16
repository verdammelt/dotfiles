;;;
;;; Modify this internal function so that tag completion is case-insensitive.
;;; Normally would do this by setting completion-ignore-case... but this
;;; function unconditionally binds that variable to nil.
;;;
(defun org-tags-completion-function (string _predicate &optional flag)
  "Complete tag STRING.
FLAG specifies the type of completion operation to perform.  This
function is passed as a collection function to `completing-read',
which see."
  (let (;; (completion-ignore-case nil)	;tags are case-sensitive ;; mjs [2023-10-14]
        (confirm (lambda (x) (stringp (car x))))
        (prefix ""))
    (when (string-match "^\\(.*[-+:&,|]\\)\\([^-+:&,|]*\\)$" string)
      (setq prefix (match-string 1 string))
      (setq string (match-string 2 string)))
    (pcase flag
      (`t (all-completions string org-last-tags-completion-table confirm))
      (`lambda (assoc string org-last-tags-completion-table)) ;exact match?
      (`nil
       (pcase (try-completion string org-last-tags-completion-table confirm)
         ((and completion (pred stringp))
          (concat prefix
                  completion
                  (if (and org-add-colon-after-tag-completion
                           (assoc completion org-last-tags-completion-table))
                      ":"
                    "")))
         (completion completion)))
      (_ nil))))
