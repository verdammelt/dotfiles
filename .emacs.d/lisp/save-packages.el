;;; save-packages.el --- save and restore installed packages

;; Copyright (C) 2012 Peter Eisentraut

;; Author: Peter Eisentraut <peter@eisentraut.org>
;; URL: https://github.com/petere/emacs-save-packages
;; Version: 0.20121012
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is a little helper to synchronize installed ELPA packages
;; across hosts.  Call `save-packages' on one host, copy the output
;; file to another host, and call `install-saved-packages' there.
;;
;; You could call `save-packages' from an idle timer or
;; `kill-emacs-hook'.  And if you are slightly daring, you could try
;; calling `install-saved-packages' from your initialization file.
;;
;; If you need something more powerful than this, you might want to
;; look into el-get.

;;; Code:

(defgroup save-packages nil
  "Save and restore installed packages."
  :group 'environment)

(defcustom save-packages-file
  (locate-user-emacs-file "saved-packages")
  "Default name of file in which to save packages."
  :type 'file
  :group 'save-packages)

(require 'cl)
(eval-when-compile (require 'package))

;;;###autoload
(defun save-packages (&optional filename)
  "Save list of currently installed packages.
The list is written to FILENAME, or `save-packages-file' by default."
  (interactive (let ((insert-default-directory nil))
                 (list (read-file-name "Save package list to file: " nil nil nil save-packages-file))))
  (with-temp-buffer
    (cl-prettyprint (cl-sort (mapcar 'car package-alist) #'string-lessp :key #'symbol-name))
    (write-region (point-min) (point-max) (or filename save-packages-file))))

;;;###autoload
(defun install-saved-packages (&optional filename)
  "Install from a saved list of packages.
Read a list of saved packages from FILENAME (`save-packages-file'
by default), and offer to install the missing packages."
  (interactive (let ((insert-default-directory nil))
                 (list (read-file-name "Load package list from file: " nil nil t save-packages-file))))
  (with-temp-buffer
    (insert-file-contents (or filename save-packages-file))
    (let* ((saved-package-list (car (read-from-string (buffer-string))))
           (missing-package-list (remove-if 'package-installed-p saved-package-list)))
      (map-y-or-n-p "Install package \"%s\"? "
                    'package-install
                    missing-package-list
                    '("package" "packages" "install")))))

;;;###autoload
(defun missing-packages (file)
  "Report a list of packages from the file which are not currently installed"
  (let ((saved-packages 
	  (car (read-from-string
		(with-temp-buffer
		  (insert-file-contents file)
		  (buffer-string))))))
    (remove-if 'package-installed-p saved-packages)))

(provide 'save-packages)

;;; save-packages.el ends here
