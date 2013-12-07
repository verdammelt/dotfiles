(require 'save-packages)
(setq save-packages-file (locate-user-emacs-file ".saved-packages"))

;;; get Package set up properly and initializedn
(require 'package)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives 
	     '("marmalade" . "http://marmalade-repo.org/packages/") 
	     :append)
(package-initialize)


;; report if there are missing packages

(if (file-exists-p save-packages-file)
    (let ((missing-packages (missing-packages save-packages-file)))
      (if (< 0 (length missing-packages))
	  (progn 
	    (message "Missing packages: %s" missing-packages)
	    (sit-for 5))))
  (save-packages save-packages-file))

(defadvice package-menu-execute (after save-package-list activate)
  "Save the new package list after a change"
  (message "Saving package list.")
  (save-packages))
(ad-activate 'package-menu-execute)

(provide 'init-package)
