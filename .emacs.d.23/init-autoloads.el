;;; init-autoloads.el
;;;
;;; Time-stamp: <2011-03-05 13:00:54 mark>
;;;
;;; Setup autoloads.
;;;

(require 'bbdb-autoloads)
(autoload 'gnus "gnus" t)
(autoload 'bbdb-expire-bbdb "bbdb-expire")
(autoload 'shell-command-completion-mode "shell-command")

(provide 'init-autoloads)
