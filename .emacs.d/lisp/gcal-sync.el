;;; gcal-sync.el --- import google calendar into our dairy  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Steve Beaulac

;; Author: Steve Beaulac <steve@sagacity.ca>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; Add the folling line to your diary file
;;; #include "~/.emacs.d/gcal-diary"
;;

;;; Code:

(defvar gcal-private-calendar-urls ())
(defvar gcal-diary-file (expand-file-name "gcal-diary" user-emacs-directory))

(defun gcal-sync--import-ical-to-diary (diary)
  "Import an ical buffer to the diary file"
  (set-buffer-multibyte t)
  (set-buffer-file-coding-system 'utf-8)
  (when (icalendar-import-buffer diary t)
    (let ((b (find-buffer-visiting diary)))
      (kill-buffer b))))

(defun gcal-sync--import-to-diary-file (url diary)
  "Fetch a ical calendar from a private google calendar url
and import it into a diary file"
  (url-retrieve url (lambda (status)
                      (when (plist-get status :error)
                        (cl-destructuring-bind (error-symbol . data)
                            (plist-get status :error)
                          (signal error-symbol data)))
                      (gcal-sync--import-ical-to-diary diary))))

(defun gcal-sync-calendars-to-diary ()
  "Fetch several google calendar (using a private ics url) and import them
into your default diary file"
  (interactive)
  (let ((urls gcal-private-calendar-urls))
    (with-temp-file gcal-diary-file (erase-buffer))
    (while urls
      (gcal-sync--import-to-diary-file (car urls) gcal-diary-file)
      (setq urls (cdr urls)))))

(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)

(provide 'gcal-sync)
;;; gcal-sync.el ends here
