;;; denote-orphans.el --- operations for dealing with orphan notes in denote  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  mark

;; Author: mark <verdammelt@gmail.com>
;; Keywords: files

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

;;

;;; Code:

;; would be nice to use:
;; (denote--retrieve-process-grep
;;  (denote-retrieve-filename-identifier f))
;; but this will repetedly get directory files...

(defun denote-orphans--inbound-links (f &optional files)
  (remove f
          (mapcar #'(lambda (item) (xref-location-group (xref-item-location item)))
                  (xref-matches-in-files
                   (denote-retrieve-filename-identifier f)
                   (or files (denote-directory-files))))))

(defvar denote-orphans--ignore-keywords '("converted"))

(defun denote-orphans--outbound-links (f)
  (with-temp-buffer
    (insert-file-contents f)
    (append
     (denote-link--collect-identifiers
      (let ((format (denote-link--file-type-format (denote-filetype-heuristics f) t)))
        (if (symbolp format) (symbol-value format) format)))
          (denote-link--collect-identifiers
      (let ((format (denote-link--file-type-format (denote-filetype-heuristics f) nil)))
        (if (symbolp format) (symbol-value format) format))))))

(defun denote-orphans--keywords-for-file (f)
  (denote-retrieve-keywords-value f (denote-filetype-heuristics f)))

(defun denote-orphans--keyword-counts ()
  (cl-reduce
   #'(lambda (counts tag) (incf (gethash tag counts -1)) counts)
   (cl-remove-if #'(lambda (k) (member k denote-orphans--ignore-keywords))
                 (apply #'append
                        (cl-remove ""
                                   (mapcar #'denote-orphans--keywords-for-file
                                           (denote-directory-files)))))
   :initial-value (make-hash-table :test 'equal)))

(defun denote-orphans--keyword-sum-for-file (f counts)
  (apply #'+ (mapcar #'(lambda (k) (gethash k counts 0))
                     (denote-orphans--keywords-for-file f))))

;; TODO: this needs a huge cleanup!
(defun denote-orphan-list-orphans ()
  "Produce a list of ORPHAN notes.

An ORPHAN note is a note that:

   1. has no links to it
   2. has no links from it
   3. has no keywords which are also on other notes.

"
  (let* ((files (denote-directory-files))
         (text-files (denote-directory-text-only-files))
         (keyword-counts (denote-orphans--keyword-counts))
         (files-and-keyword-count
          (mapcar #'(lambda (f) (cons f (denote-orphans--keyword-sum-for-file f keyword-counts)))
                  files))
         (files-with-unlinked-keywords
          (mapcar #'car (cl-remove-if-not #'zerop files-and-keyword-count :key #'cdr)))
         (files-and-outbound
          (mapcar #'(lambda (f) (cons f (denote-orphans--outbound-links f)))
                  files-with-unlinked-keywords))
         (files-without-outbound-links
          (mapcar #'car (cl-remove-if-not #'null files-and-outbound :key #'cdr)))
         (files-and-inbound
          (mapcar #'(lambda (f) (cons f (denote-orphans--inbound-links f text-files)))
                  files-without-outbound-links))
         (orphans
          (cl-remove-if-not #'null files-and-inbound :key #'cdr)))
    orphans))

(defun denote-orphans-visit-orphan ()
  "Prompt user for an ORPHAN note to visit.
See DENOTE-ORPHAN-LIST-ORPHANS for defintion of ORPHAN."
  (interactive)
  (message "...computing orphans...")
  (let ((orphans (denote-orphan-list-orphans)))
    (find-file
     (expand-file-name
      (completing-read "Choose orphan node: "  orphans nil t)
      denote-directory))))

(provide 'denote-orphans)
;;; denote-orphans.el ends here
