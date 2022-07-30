;;;;
;;;; BBDB
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;
;;; TODO
;;; * safer sync with google (merge in google contacts)
;;; * two way sync?
;;; * expiry
;;;
(use-package bbdb
  :bind (:map bbdb-mode-map
              ("r" . bbdb-merge-records)
              ("&" . #'mjs/bbdb-recursive-search))
  :hook ((bbdb-afer-change . (lambda (arg) (bbdb-save)))
         (bbdb-notice-record . (lambda (record) (message "NOTICED %s <%s>"
                                                    (bbdb-record-name record)
                                                    (bbdb-record-mail record)))))
  :config
  (progn
    (declare-function bbdb-save "bbdb")
    (declare-function bbdb-record-name "bbdb")
    (setq bbdb-pop-up-window-size 10
          bbdb-mua-pop-up-window-size 5
          bbdb-mua-interactive-action  '(query . create)
          bbdb-message-all-addresses t
          bbdb-complete-mail-allow-cycling t)

    (advice-add 'bbdb-records :around #'mjs/bbdb-records-around)))

(defun mjs/bbdb-init (package)
  (message "Initialize BBDB for %s" package)
  (bbdb-initialize package)
  (bbdb-mua-auto-update-init package))

(defvar mjs/bbdb-recursive-search-p nil "Set to T to cause next search to be recursive")

(declare-function bbdb-prefix-message "bbdb-com")

(defun mjs/bbdb-recursive-search (&optional arg)
  "Toggle the recursive search flag"
  (interactive "P")
  (setq mjs/bbdb-recursive-search-p
        (or (and (numberp arg) (< 0 arg))
            (and (not (numberp arg)) (not mjs/bbdb-recursive-search-p))))
  (aset bbdb-modeline-info 4 (if mjs/bbdb-recursive-search-p "rec"))
  (aset bbdb-modeline-info 5
        (if mjs/bbdb-recursive-search-p
            (substitute-command-keys
             "\\<bbdb-mode-map>\\[mjs/bbdb-recursive-search]")))
  (bbdb-prefix-message))

(defun mjs/bbdb-records-around (old-func)
  "If recursive search in effect then return current records only,
otherwise return all records (as normal)"
  (if mjs/bbdb-recursive-search-p
      (progn (mjs/bbdb-recursive-search nil) (mapcar #'car bbdb-records))
    (progn (funcall old-func))))
