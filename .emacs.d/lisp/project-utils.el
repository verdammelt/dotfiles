;;; project-utils.el --- utility functions / tweaks for project  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Mark Simpson

;; Author: Mark Simpson <mark.simpson@defmethod.com>
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

;;; Replacement function for project-switch-project which wraps teardown & setup
;;; hooks around switching the project. This allows for running commands within
;;; the old project being switched from and also in the new project being
;;; switched to.

;;; Definition of a 'local' project type which is defined as having a .project
;;; or .projectile file in it

;;; Code:

(defvar mjs/project-setup-hook nil
  "Functions to call when a new project is switched to.
MJS/CURRENT-PROJECT will contain the project that was just
switched to.")

(defvar mjs/project-teardown-hook nil
  "Functions to call wwhen a new porject is being switched away from.
MJS/CURRENT-PROJECT will contain the project that is being
switched away from.")

(defvar mjs/current-project nil
  "Holds the current project that has most recently been switched to.")

(defun mjs/project-switch-project (dir)
  "Wrapper around PROJECT-SWITCH-PROJECT to allow for
setup/teardown of the projects."
  (interactive (list (project-prompt-project-dir)))

  ;; teardown old project
  (run-hooks 'mjs/project-teardown-hook)
  (setq mjs/current-project nil)

  (project-switch-project dir)

  ;; setup new project
  (setq mjs/current-project (project-current nil dir))
  (run-hooks 'mjs/project-setup-hook))

;;;
;;; Local Project type
;;;
;;; A local project is a directory which contains either a .project or
;;; .projectile file and all the subdirectories there of.
;;;
;;; This try function should be added to project-find-functions at or near the
;;; end so it can be used as 'last chance' type of project.
;;;
(defun mjs/project-try-local (dir)
  "Determine if DIR is a non-VC project.
DIR must include a .project or .projectile file to be considered
a project."
  (if-let ((root (seq-some (lambda (n)
                             (locate-dominating-file dir n))
                           '(".project" ".projectile"))))
      (cons 'local root)))

(cl-defmethod project-root ((project (head local))) (cdr project))

(provide 'project-utils)
;;; project-utils.el ends here
