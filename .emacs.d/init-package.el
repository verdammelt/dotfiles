;;;;
;;;; Package Setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(require 'package)

(advice-add 'package--save-selected-packages :filter-args #'mjs/sort-packages)

(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(setq package-archive-priorities '(("gnu" . 10) ("nongnu" . 10)
                                   ("melpa-stable" . 1)
                                   ("melpa" . 0)))

(defun mjs/perform-updates ()
  (interactive)
  (progn
    (save-some-buffers 'save-silently)
    (package-refresh-contents)
    (package-show-package-list)
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

;;; USE-PACKAGE boostrapping
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile (require 'use-package))
(use-package use-package
  :init
  (setq use-package-always-ensure t
        use-package-always-defer t
        use-package-verbose t))
