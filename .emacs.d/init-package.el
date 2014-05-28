;;;;
;;;; Package Setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Modified Time-stamp: <2014-05-27 21:17:35 mark>
;;;;
(require 'cl)

(setq message-log-max 10000)

;;; get Package set up properly and initialized
(require 'package)
(add-to-list 'package-archives 
	     '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(defvar needed-packages
  '(ac-ispell
    ace-jump-mode
    auctex
    bbdb
    browse-kill-ring
    clojure-mode
    coffee-mode
    expand-region
    fill-column-indicator
    flx-ido
    git-commit-mode
    git-rebase-mode
    ido-hacks
    ido-sort-mtime
    ido-ubiquitous
    ido-vertical-mode
    magit
    markdown-mode
    org
    paredit
    projectile
    rainbow-delimiters
    slime
    smex
    solarized-theme
    yasnippet
    ))

(defun missing-packages ()
  (remove-if #'package-installed-p needed-packages))

(defun install-missing-packages ()
  (let ((missing-packages (missing-packages)))
    (when (and missing-packages
	       (yes-or-no-p 
		(format "Install missing packages: %s" missing-packages)))
      (package-refresh-contents)
      (mapc #'(lambda (p) 
		(message "Installing %s" p)
		(package-install p)) missing-packages))))

(install-missing-packages)

(provide 'init-package)
