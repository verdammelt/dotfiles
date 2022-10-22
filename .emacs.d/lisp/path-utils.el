;;; path-utils.el --- utility functions for modifying exec-path/PATH  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Mark Simpson

;; Author: Mark Simpson <mjs@theworld.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

;;
;; General functions for operating on exec-path / PATH
;;
(defun mjs/add-to-path (dir)
  (cl-pushnew dir exec-path :test #'string=)
  (mjs/set-path-envvar-from-exec-path))

(defun mjs/remove-from-path (dir)
  (setq exec-path (cl-remove dir exec-path :test #'string=))
  (mjs/set-path-envvar-from-exec-path))

(defun mjs/set-path-envvar-from-exec-path ()
  (setenv "PATH" (mapconcat 'identity exec-path ":")))


;;;
;;; Deducing paths to add/remove
;;;
;; NVM
(defun mjs/remove-node-from-path ()
  "Remove NVM_BIN from exec-path."
  (mjs/remove-from-path (file-name-as-directory (getenv "NVM_BIN"))))

(defun mjs/add-node-to-path ()
  "Use NVM to detect the version of node and add it to exec-path."
  (cond ((file-exists-p ".nvmrc")
         (nvm-use-for "."))
        ((nvm--installed-versions)
         (nvm-use (car (first (cl-sort (nvm--installed-versions)
                                       #'string< :key #'first))))))
  (mjs/set-path-envvar-from-exec-path))

;; node_modules/.bin
(defun mjs/remove-node-modules-from-path ()
  "Remove current node_module/.bin directory from the path"
  (when-let ((node-module-bin
              (locate-dominating-file default-directory "node_modules/.bin/")))
    (mjs/remove-from-path (expand-file-name "node_modules/.bin/" node-module-bin))))

(defun mjs/add-node-modules-in-path ()
  "Add node_modules/.bin to exec-path and PATH if one is found."
  (when-let ((node-module-bin
              (locate-dominating-file default-directory "node_modules/.bin/")))
    (mjs/add-to-path (expand-file-name "node_modules/.bin/" node-module-bin))))

(provide 'path-utils)
;;; path-utils.el ends here
