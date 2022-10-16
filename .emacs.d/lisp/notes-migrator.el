;;; notes-migrator.el --- Migrate notes from org-roam to denote

;; Copyright (C) 2022 bitspook <bitspook@proton.me>, Mark Simpson <verdammelt@gmail.com>

;; Author: bitspook <bitspook@proton.me>, Mark Simpson <verdammelt@gmail.com>
;; Version: 0.2
;; URL: (original version) https://github.com/bitspook/notes-migrator

;;; Commentary:
;; Migrate org-roam notes to denote. It does not make any changes to org-roam
;; notes, but migrated-notes are saved in denote-directory while overwriting any
;; conflicting files.

;;; Code:
(require 'denote)
(require 'org-roam)
(require 'org-element)

(defun nm--roam-node-ctime (node)
  "Get create-time of org-roam NODE.
It assumes that date is stored in the filename of NODE in one of
the 3 formats:
- YYYY-MM-DD.org (e.g in case of org-roam-dailies)
- YYYYMMDDHHMMSS.*.org (new org-roam nodes)
- YYYY-MM-DD--HH-MM-SS.*.org (old org-roam nodes)

If no time can be found in the filename a artifcial time will be
created by taking '1970-01-01' as the DATE and the current time
of day as the TIME. (This is done to be at least mostly-unique.)"
  (let* ((fname (file-name-base (org-roam-node-file node)))
         (old-date-rx (rx (group (= 4 num) "-" (= 2 num) "-" (= 2 num))
                          "--" (group (= 2 num)) "-" (group (= 2 num)) "-" (group (= 2 num)) "Z"))
         (new-date-rx (rx (group (= 4 num)) (group (= 2 num)) (group (= 2 num))
                          (group (= 2 num)) (group (= 2 num)) (group (= 2 num)) "-"))
         (dailies-date-rx (rx (= 4 num) "-" (= 2 num) "-" (= 2 num)))
         (time-str (save-match-data
                     (or (and (string-match old-date-rx fname)
                              (concat (match-string 1 fname) "T"
                                      (format "%s:%s:%s" (match-string 2 fname) (match-string 3 fname) (match-string 4 fname))))
                         (and (string-match new-date-rx fname)
                              (format "%s-%s-%sT%s:%s:%s"
                                      (match-string 1 fname) (match-string 2 fname) (match-string 3 fname)
                                      (match-string 4 fname) (match-string 5 fname) (match-string 6 fname)))
                         (and (string-match dailies-date-rx fname)
                              (format "%sT00:00:00" (match-string 0 fname)))))))
    (when (not time-str)
      (sleep-for 1)
      (setq time-str (format-time-string "1970-01-01T%T" (current-time)))
      (warn "Encountered org-roam file with unknown name: %s.org using time: %s" fname time-str))
    (encode-time (parse-time-string time-str))))

(defun nm--roam-node-denote-id (node)
  "Create denote identifier for org-roam NODE.
It returns creation timestamp of NODE, which is obtained using
`nm--roam-node-ctime'."
  (format-time-string denote-id-format (nm--roam-node-ctime node)))

(defun nm-convert-roam-links-to-denote ()
  "Convert all org-roam links in `current buffer` to denote links."
  (interactive)
  (let* ((file-to-nodes (nm--file-to-roam-node-map))
         (file-to-ids (nm--file-to-ids-map file-to-nodes))
         (roam-to-denote-id (nm--roam-to-denote-id-map file-to-ids)))
    (nm--convert-roam-links-to-denote roam-to-denote-id (buffer-file-name))))

(defun nm--convert-roam-links-to-denote (roam-id-to-denote-id-map &optional filename)
  "Convert all org-roam links in `current-buffer' to denote links.
If org-roam node for a link is not found, a warning is logged and
the link is not converted.
FILENAME can be optionally provided for debugging in case of
failed link conversions."
  (let ((roam-link-rx (rx "[[id:")))
    (while (re-search-forward roam-link-rx nil t)
      (let* ((el (org-element-copy (org-element-context)))
             (node-id (org-element-property :path el))
             (denote-id (gethash node-id roam-id-to-denote-id-map)))
        (if (not denote-id)
            (warn "Failed to convert org-roam link to denote because corresponding denote id wasn't found. [roam-id=%s, filename=%s]" node-id filename)
          (let* ((begin (org-element-property :begin el))
                 (end (org-element-property :end el)))
            (replace-string (format "id:%s" node-id)
                            (format "denote:%s" denote-id)
                            nil begin end)))))))

(defun nm--file-to-roam-node-map ()
  "Create a hash-table that maps roam filenames to roam nodes
contained in that file."
  (cl-reduce
   #'(lambda (acc node)
       (push node (gethash (org-roam-node-file node) acc (list))) acc)
   (org-roam-node-list)
   :initial-value (make-hash-table :test 'equal)))

(defun nm--roam-node-list-by-file ()
  "Return a list of org-roam NODE objects. Return only a single
NODE per FILE. Picks the first NODE in the file (from the top) in
the case of multiples."
  (let ((node-list (list)))
    (maphash #'(lambda (k v) (push (first (cl-sort v #'< :key #'org-roam-node-point)) node-list))
             (nm--file-to-roam-node-map))
    node-list))

(defun nm--file-to-ids-map (file-node-map)
  (cl-reduce #'(lambda (map file)
                 (let ((denote-id (nm--roam-node-denote-id
                                   (first (gethash file file-node-map))))
                       (roam-ids (cl-remove-duplicates
                                  (mapcar #'org-roam-node-id (gethash file file-node-map))
                                  :test #'string=)))
                   (setf (gethash file map) (cl-pairlis '(:denote :roam)
                                                        (list denote-id roam-ids)))
                   map))
             (hash-table-keys file-node-map)
             :initial-value (make-hash-table :test 'equal)))

(defun nm--roam-to-denote-id-map (file-ids-map)
  (cl-reduce #'(lambda (map ids)
                 (let ((denote-id (cdr (assoc :denote ids)))
                       (roam-ids (cdr (assoc :roam ids))))
                   (mapc #'(lambda (roam-id) (setf (gethash roam-id map) denote-id)) roam-ids)
                   map))
             (hash-table-values file-ids-map)
             :initial-value (make-hash-table :test 'equal)))

(defun nm--insert-old-node (node-file)
  "Inserts contents of NODE-FILE into current buffer and removes
node's property drawer as well as title line."
  (goto-char (point-max))
  (forward-line -1)
  (insert-file-contents node-file)
  (kill-region (point) (search-forward ":END:\n" nil nil))
  (let ((kill-whole-line t)) (kill-line)))

(defun nm--migrate-roam-node (node roam-id-to-denote-id-map)
  (let ((keywords (cons "converted" (org-roam-node-tags node)))
        (old-file (org-roam-node-file node))
        (title (org-roam-node-title node))
        (denote-id (gethash (org-roam-node-id node) roam-id-to-denote-id-map)))

    (when-let ((denote-file (denote-get-path-by-id denote-id)))
      (warn "DELETING existing Denote file for id %s: %s" denote-id denote-file)
      (delete-file denote-file t))

    (denote title keywords "org" nil denote-id)
    (nm--insert-old-node old-file)
    (nm--convert-roam-links-to-denote roam-id-to-denote-id-map old-file)

    (save-buffer)
    (message "Migrate org-roam file %s to denote file %s" old-file (buffer-file-name))
    (kill-buffer)))

(defun nm-migrate-roam-to-denote ()
  (let* ((file-to-nodes (nm--file-to-roam-node-map))
         (file-to-ids (nm--file-to-ids-map file-to-nodes))
         (roam-to-denote-id (nm--roam-to-denote-id-map file-to-ids))
         (nodes (nm--roam-node-list-by-file)))
    (dolist (node nodes)
      (nm--migrate-roam-node node roam-to-denote-id))))

(provide 'notes-migrator)
;;; notes-migrator.el ends here
