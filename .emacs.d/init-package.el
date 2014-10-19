;;;;
;;;; Package Setup
;;;;
;;;; [if found please return to damned@theworld.com]
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

(setq mjs/*needed-packages*
  '(ac-cider
    ac-ispell
    ace-jump-mode
    auctex
    auto-complete
    bbdb
    browse-kill-ring
    cider
    change-inner
    clojure-mode
    coffee-mode
    elisp-slime-nav
    epl
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
    miniedit
    multiple-cursors
    org-plus-contrib
    paredit
    pretty-mode
    projectile
    rainbow-delimiters
    slime
    smex
    solarized-theme
    wrap-region
    wgrep
    wrap-region
    yasnippet
    ))

(defun mjs/missing-packages ()
  "Return a list of needed packages which are not installed."
  (remove-if #'package-installed-p mjs/*needed-packages*))

(defun mjs/install-missing-packages ()
  "Install any needed packages which are not installed."
  (let ((missing-packages (mjs/missing-packages)))
    (when (and missing-packages
	       (yes-or-no-p 
		(format "Install missing packages: %s" missing-packages)))
      (package-refresh-contents)
      (mapc #'(lambda (p) 
		(message "Installing %s" p)
		(package-install p)) missing-packages))))

(mjs/install-missing-packages)

(defun mjs/package-from-requirement (req) 
  "Returns a package structure for the given requirement. 
NOTE: assumes that the package can be found amongst the installed packages."
  (epl-find-installed-package (epl-requirement-name req)))

(defun mjs/required-packages (package)
  "Returns a list of packgaes required by the given package."
  (mapcar #'mjs/package-from-requirement (epl-package-requirements package)))

(defun mjs/all-required-packages-for (package)
  "Compute all the required packages (inluding requirements of
requirements etc) for the given package."
  (cond ((null package) nil)
	(t (let ((reqs (remove nil (mjs/required-packages package))))
	     (delete-duplicates
	      (append (list package) reqs
		      (mapcan #'mjs/all-required-packages-for reqs)))))))

(defun mjs/extra-packages ()
  "List all installed packages which are not in the mjs/*needed-packages* list."
  (let ((all-needed 
	 (delete-duplicates 
	  (mapcar #'epl-package-name 
		  (mapcan #'mjs/all-required-packages-for
			  (remove nil (mapcar #'epl-find-installed-package mjs/*needed-packages*))))))
	(installed (mapcar #'epl-package-name (epl-installed-packages))))
    (remove-if #'(lambda (p) (member p all-needed)) installed)))

(defun mjs/find-upgrades ()
  "Find all needed packages that have an available upgrade."
  (let ((upgrades 
	 (epl-find-upgrades 
	  (remove nil (mapcar #'epl-find-installed-package mjs/*needed-packages*)))))
    upgrades))

(defun mjs/perform-updates ()
  (interactive)
  (epl-refresh)
  (let ((needing-updates (mapcar #'epl-upgrade-installed (mjs/find-upgrades))))
    (if (and needing-updates 
	     (y-or-n-p 
	      (format "Upgrade these packages? %S" (mapcar #'epl-package-name needing-updates))))
	(epl-upgrade needing-updates)))
  (let ((extra-packages (mjs/extra-packages)))
    (if (and extra-packages
	     (y-or-n-p
	      (format "Delete any extra packages? %S" extra-packages)))
	(mapcar #'(lambda (p)
		    (if (y-or-n-p (format "Delete %S" p))
			(epl-package-delete (epl-find-installed-package p))))
		extra-packages))))
