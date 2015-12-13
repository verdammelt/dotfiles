;;;;
;;;; Package Setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(require 'cl-lib)

(setq message-log-max 10000)

;;; get Package set up properly and initialized
(require 'package)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(defvar mjs/*needed-package-names*
  '(ac-cider
    ac-ispell
    ac-slime
    auctex
    auto-complete
    avy
    bbdb
    browse-kill-ring
    cider
    change-inner
    clj-refactor
    clojure-mode
    coffee-mode
    diminish
    edit-server
    edit-server-htmlize
    elisp-slime-nav
    expand-region
    fill-column-indicator
    flycheck
    flx-ido
    fullframe
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
    projectile
    rainbow-delimiters
    rvm
    slime
    smex
    virtualenvwrapper
    wgrep
    wrap-region
    yasnippet
    ))


(defun mjs/find-installed-package (name)
  "Returns the newest package description matching NAME from
PACKAGE-ALIST or PACKAGE--BUILTINS."
  (if (package-built-in-p name)
      (package--from-builtin (assq name package--builtins))
    (cadr (assq name package-alist))))

(defun mjs/find-available-package (name)
  (cadr (assq name package-archive-contents)))

(defun mjs/needed-packages ()
  (mapcar #'mjs/find-installed-package
          mjs/*needed-package-names*))

(defun mjs/missing-packages ()
  (cl-remove-if #'package-installed-p
                mjs/*needed-package-names*))

;;;
;;; Extra package detection and cleanup
;;;
(defun mjs/required-packages (package)
  "Returns a list of packages required by the given package."
  (cl-flet ((package-from-requirement
             (req)
             (mjs/find-installed-package (car req))))
    (mapcar #'package-from-requirement (package-desc-reqs package))))

(defun mjs/all-required-packages-for (package)
  "Compute all the required packages (inluding requirements of
requirements etc) for the given package."
  (if (null package) nil
    (let ((reqs (mjs/required-packages package)))
      (cl-delete-duplicates
       (append (list package) reqs
               (cl-mapcan #'mjs/all-required-packages-for reqs))))))

(defun mjs/extra-packages ()
  "List all installed packages which are not in the
mjs/*needed-package-names* list."
  (let ((all-needed
         (cl-delete-duplicates
          (mapcar #'package-desc-name
                  (cl-mapcan #'mjs/all-required-packages-for
                             (mjs/needed-packages)))))
        (installed (mapcar #'car package-alist)))
    (cl-remove-if #'(lambda (p) (member p all-needed)) installed)))

(defun mjs/remove-extra-packages ()
  (let ((extra-packages (mjs/extra-packages)))
    (if (and extra-packages
             (y-or-n-p
              (format "Delete any extra packages? %S" extra-packages)))
        (mapcar #'(lambda (p)
                    (if (y-or-n-p (format "Delete %S" p))
                        (package-delete (mjs/find-installed-package p))))
                extra-packages))))

;;;
;;; Upgrading
;;;
(defun mjs/find-upgrades ()
  "Find all needed packages that have an available upgrade."
  (let ((upgrades (list)))
    (dolist (pkg (mjs/needed-packages) upgrades)
      (cl-flet ((package-version (p) (when p (package-desc-version p))))
        (let ((available-pkg
               (mjs/find-available-package (package-desc-name pkg))))
          (when (version-list-<
                 (package-version pkg)
                 (package-version available-pkg))
            (push available-pkg upgrades)))))))

(defun mjs/upgrade-packages ()
  (let ((needing-updates (mjs/find-upgrades)))
    (if (and needing-updates
             (y-or-n-p
              (format "Upgrade these packages? %S"
                      (mapcar #'package-desc-name needing-updates))))
        (dolist (pkg needing-updates) (package-install pkg)))))

;;;
;;; Extra Version Cleanup
;;;
(defun mjs/remove-extra-versions ()
  (cl-flet ((has-only-one-version (p) (= (length (cdr p)) 1))
            (old-versions (p) (cl-copy-list (cddr p))))
    (let ((extra-versions (cl-remove-if
                           #'has-only-one-version
                           package-alist)))
      (if (and extra-versions
               (y-or-n-p
                (format "Delete old package versions? %S"
                        (mapcar #'car extra-versions))))
          (mapcar #'package-delete
                  (cl-mapcan #'old-versions extra-versions))))))

;;;
;;; Installing Missing Packages
;;;
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

;;;
;;; Perform Updates/Cleanups
;;;
(defun mjs/perform-updates ()
  (interactive)
  (package-refresh-contents)

  (message "Starting Updates")
  (mjs/install-missing-packages)
  (mjs/upgrade-packages)
  (mjs/remove-extra-packages)
  (mjs/remove-extra-versions)
  (message "Updates completed"))

(mjs/install-missing-packages)
