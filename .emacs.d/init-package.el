;;;;
;;;; Package Setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(require 'cl-lib)

;;; get Package set up properly and initialized
(require 'package)

(defun mjs/perform-updates ()
  (interactive)
  (progn
    (package-refresh-contents)
    (package-show-package-list)
    (let ((package-menu-async nil)) (package-menu-refresh))
    (when (package-menu--find-upgrades) (package-menu-mark-upgrades))
    (ignore-errors (package-menu-execute t))
    (package-autoremove)
    (quit-window)))

(defun mjs/sort-packages (&optional value)
  ;; Must return a list because this we are going to call (APPLY OLDFUNC VALUE)
  ;; with this return value.
  (list
   (cl-sort (copy-sequence (or (car value) package-selected-packages))
            #'string<
            :key #'symbol-name)))

(advice-add 'package--save-selected-packages :filter-args #'mjs/sort-packages)

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(package-install-selected-packages)

(when (not (package-installed-p 'use-package))
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package)
  (setq use-package-always-ensure t
        use-package-verbose t))
