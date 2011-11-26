;;;
;;; Dired Setups
;;;
;;; Time-stamp: <2007-10-20 21:29:17 damned>

(require 'dired)
(require 'ls-lisp)

(setq dired-listing-switches "-aFlh"
      dired-recursive-copies 'top
      dired-recursive-deletes 'top
      directory-free-space-args "-Pkh"
      list-directory-verbose-switches "-lFh")

(provide 'init-dired)