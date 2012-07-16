;; google-contacts.el
;; -*- coding: utf-8 -*-
;;
;; Author: Øyvind Stegard <oyvind.stegard@ifi.uio.no>
;; License: Public domain, use at own risk, no warranties of any kind.
;;
;; Version 0.2.2-alpha
;; - Fetch contacts from Google using GData APIs in JSON format.
;; - Support for merging fetched contacts with BBDB.
;;
;; Basic usage:
;; - Set variables `google-contacts-email' and `google-contacts-passwd'.
;; - Call function `google-contacts-retrieve'.
;;   It returns a simple Lisp structure which should contain all your contacts.
;;
;; Be careful with the `google-contacts-merge-with-bbdb' function. It will
;; replace all contacts you have in your BBDB address book for which there
;; exists a Google contact, with only data from the Google contact. This is
;; because I mostly edit contacts in GMail and just export to BBDB when
;; necessary, using this function. Also bear in mind that this is *alpha* code,
;; tailored to my own needs. It was written using Emacs 23, your milage may vary
;; if you're using a different Emacs flavour or version.
;;
;;** BACKUP YOUR BBDB DATABASE BEFORE TESTING THIS FUNCTION **
;;
;; TODO Document Lisp structure returned from `google-contacts-retrieve'
;; TODO General clean up, clean up BBDB merge code.
;;
;; Changelog:
;; * v0.2.2-alpha, 2010-11-28
;;   Add explicit requirement on common lisp extensions.
;; 
;; * v0.2.1-alpha, 2010-10-13
;;   Fixed problem with contacts that have no emails and zero-length title/name
;;   string.
;;

(require 'bbdb)
(require 'auth-source)
(require 'json)
(require 'url)
(require 'tls)
(require 'cl)

(defvar google-contacts-email nil "GMail address")
(defvar google-contacts-passwd nil "GMail password")

(defvar google-contacts-passwd-use-auth-source nil
  "If t, then look in default auth source for GMail credentials (netrc machine id 'gmail.com')")

(defvar google-contacts-system-group-names
  '((friends . "Friends")
    (coworkers . "Coworkers")
    (family . "Family")) "Local names for Google fixed system groups")


(defun google-contacts-get-credentials ()
  (let (email passwd)
    (if google-contacts-passwd-use-auth-source
        (setq email (auth-source-user-or-password "login" "gmail.com" nil)
              passwd (auth-source-user-or-password "password" "gmail.com" nil))
      (setq email google-contacts-email passwd google-contacts-passwd))
    (if (and email passwd)
        (cons email passwd)
      (error "Email/password not set or not found in auth source for host 'gmail.com'."))))

(defun google-contacts-clientlogin ()
  "Login to Google contacts service, obtain auth cookie which is returned as a string."
  (let* ((creds (google-contacts-get-credentials))
         (email-encoded (url-hexify-string (car creds)))
         (passwd-encoded (url-hexify-string (cdr creds)))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-type" . "application/x-www-form-urlencoded")))
         (tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof")) ;; gnutls-cli just hangs, for unknown reason. Use openssl s_client ..
         (url-request-data (concat "Email=" email-encoded "&Passwd=" passwd-encoded "&service=cp&source=Emacs"))
         auth-cookie)
    (with-current-buffer
        (with-timeout (10 (error "Google contacts: ClientLogin authentication took too long, aborting"))
          (url-retrieve-synchronously "https://www.google.com/accounts/ClientLogin"))
      (goto-char (point-min))
      (re-search-forward "^Auth=\\(.*\\)" nil t)
      (setq auth-cookie (match-string-no-properties 1))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))
    auth-cookie))

(defun google-contacts-retrieve-all-as-json (auth-cookie)
  "Fetch all contacts from Google and return as a parsed JSON object (Lisp structure)"
  (let ((url-request-extra-headers (list (cons "Authorization" (concat "GoogleLogin auth=" auth-cookie))
                                         (cons "GData-Version" "3.0"))) ;; Use GData version 3 to get nick-names
        json)
    
    (with-current-buffer
        (with-timeout (10 (error "Google contacts: connection or transfer to took too long, aborting"))
          (url-retrieve-synchronously "http://www.google.com/m8/feeds/contacts/default/full?alt=json&max-results=1000"))
      (declare (special url-http-end-of-headers))
      (set-buffer-multibyte t)
      (decode-coding-region (1+ url-http-end-of-headers) (point-max) 'utf-8)
      (goto-char (1+ url-http-end-of-headers))
      (setq json (json-read-object))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))
    json))

(defun google-contacts-retrieve-groups (auth-cookie)
  (let ((url-request-extra-headers (list (cons "Authorization" (concat "GoogleLogin auth=" auth-cookie))
                                         (cons "GData-Version" "2.0"))) ;; Use GData version 2.0 to get system groups.
                                                                      ;; They aren't returned with version 3.0, for some unknown reason.
        json)
    (with-current-buffer
        (with-timeout (10 (error "Google contacts: connection or transfer to took too long, aborting."))
          (url-retrieve-synchronously "http://www.google.com/m8/feeds/groups/default/full?alt=json&max-results=1000"))
      (declare (special url-http-end-of-headers))
      (set-buffer-multibyte t)
      (decode-coding-region (1+ url-http-end-of-headers) (point-max) 'utf-8)
      (goto-char (1+ url-http-end-of-headers))
      (setq json (json-read-object))
      (set-buffer-modified-p nil)
      (kill-buffer (current-buffer)))
    (when (not json)
        (error "Could not retrieve contact groups as JSON"))
    (let ((entries (cdr (assoc-string 'entry (assoc-string 'feed json))))
          groups-alist
          entrynode)
      (loop for entrynode across entries
            do
            (let ((title (cdr (assoc-string "$t" (cdr (assoc-string "title" entrynode)))))
                  (id (cdr (assoc-string "$t" (cdr (assoc-string "id" entrynode)))))
                  )
              (when (string-match "^System Group:" title)
                (cond
                 ((string-match "^System Group: My Contacts" title)
                  (setq title nil))
                 ((string-match "^System Group: Friends" title)
                  (setq title (cdr (assoc 'friends google-contacts-system-group-names))))
                 ((string-match "^System Group: Family" title)
                  (setq title (cdr (assoc 'family google-contacts-system-group-names))))
                 ((string-match "^System Group: Coworkers" title)
                  (setq title (cdr (assoc 'coworkers google-contacts-system-group-names))))
                 nil)
               )
              (when title
                (setq groups-alist (cons (cons id title) groups-alist)))
              )
            )
      groups-alist)))

(defun google-contacts-normalize-whitespace(s)
  (and s (replace-regexp-in-string "^ \\| $" "" (replace-regexp-in-string "[ \t]+" " " s))))
  
;; Used for finding common email address(es) between two sets/bags in contact
;; matching between Google contacts and BBDB.
(defun google-contacts-intersection-ignore-case(list1 list2)
  (cond
   ((not list1) nil)
   ((member-ignore-case (car list1) list2)
    (cons (car list1) (google-contacts-intersection-ignore-case (cdr list1) list2)))
   (t (google-contacts-intersection-ignore-case (cdr list1) list2))))

;; Mappings from Google location type schema to symbol (used for addresses and phone numbers)
(setq google-contacts-location-schema-mapping
      '(("http://schemas.google.com/g/2005#main" . main)
        ("http://schemas.google.com/g/2005#work" . work)
        ("http://schemas.google.com/g/2005#home" . home)
        ("http://schemas.google.com/g/2005#mobile" . mobile)
        ("http://schemas.google.com/g/2005#other" . other)))

;; Get plain phone number structure from phone-node in JSON structure
(defun google-contacts-get-phone-numbers(phone-node)
  (when phone-node
    (map 'list
         (lambda(phone-number)
           (let ((number (cdr (assoc '$t phone-number)))
                 (location (cdr (assoc-string (cdr (assoc 'rel phone-number))
                                          google-contacts-location-schema-mapping))))
             (when (not location) (setq location 'other))
             (cons location number)))
         (cdr phone-node))))

;; Get plain address structure from address-node in JSON structure
(defun google-contacts-get-addresses(address-node)
  (when address-node
    (map 'list
         (lambda(address)
           (let ((location (cdr (assoc-string (cdr (assoc 'rel address))
                                          google-contacts-location-schema-mapping)))
                 (formatted-address (cdr (assoc '$t (assoc 'gd$formattedAddress address)))))
             (when (not location) (setq location 'other))
             (cons location formatted-address)))
         (cdr address-node))))

;; Get list of group names (strings) from group membership node in JSON structure
(defun google-contacts-get-groups(group-membership-node groups-id-name-alist)
  (delete nil (mapcar (lambda(e)
                        (cdr (assoc-string (cdr (assoc 'href e)) groups-id-name-alist)))
                      (cdr group-membership-node))))

;; Get company name from organization-node in JSON structure
(defun google-contacts-get-company (organization-node)
  (when organization-node
    (setq organization-node (cdr organization-node))
    (when (> (length organization-node) 0)
      (cdr (assoc '$t (assoc 'gd$orgName (aref organization-node 0)))))))

;; Retrieves contacts from GMail and returns simple Lisp structure.
(defun google-contacts-retrieve ()
  "Returns Google contacts as alist"
  (let ((auth-cookie (google-contacts-clientlogin))
        google-contacts-alist
        groups-id-name-alist
        json)
    (when auth-cookie
      (setq json (google-contacts-retrieve-all-as-json auth-cookie))
      (when (not json)
        (error "Could not retrieve contacts as JSON"))
      (setq groups-id-name-alist (google-contacts-retrieve-groups auth-cookie))
      (setq json (cdr (car (cdr (car json))))) ;; Untangle top structural elements of JSON object
      (loop for contactnode across json
            do
            (let* ((email-node (assoc 'gd$email contactnode))
                   (title-node (assoc 'title contactnode)) ; consider using gd$fullName node instead
                   (address-node (assoc 'gd$structuredPostalAddress contactnode))
                   (phone-node (assoc 'gd$phoneNumber contactnode))
                   (organization-node (assoc 'gd$organization contactnode))
                   (nickname-node (assoc 'gContact$nickname contactnode))
                   (birthday-node (assoc 'gContact$birthday contactnode))
                   (group-membership-node (assoc 'gContact$groupMembershipInfo contactnode))
                   (content-node (assoc 'content contactnode)) ; contact notes-field
                   
                   (name
                    (and title-node (google-contacts-normalize-whitespace (cdr (assoc-string '$t title-node)))))
                   (emails
                    (and email-node (map 'list (lambda(e) (cdr (assoc-string 'address e))) (cdr email-node))))

                   contact addresses
                   phone-numbers company
                   nickname birthday groups notes)
              ;; Ignore all contacts with no emails and no name:
              (when (or emails (and name (> (length name) 0)))
                (setq phone-numbers (google-contacts-get-phone-numbers phone-node))
                (setq company (google-contacts-get-company organization-node))
                (setq addresses (google-contacts-get-addresses address-node))
                (setq nickname (cdr (assoc-string '$t nickname-node)))
                (setq birthday (cdr (assoc-string 'when birthday-node)))
                (setq groups (google-contacts-get-groups group-membership-node groups-id-name-alist))
                (setq notes (cdr (assoc '$t (cdr content-node))))
                (when (and nickname (> (length nickname) 0))
                  (setq contact (cons (cons 'aka nickname) contact)))
                (when birthday
                  (setq contact (cons (cons 'birthday birthday) contact)))
                (when notes
                  ;; HTC Android phones store additional tagged metadata in the notes field, strip that away.
                  (setq notes (replace-regexp-in-string "<HTCData>\\(.\\|\n\\)*?</HTCData>" "" notes))
                  (setq notes (replace-regexp-in-string "\\`\\( \\|\n\\|\t\\)+\\|\\( \\|\n\\|\t\\)+\\'" "" notes))
                  (when (> (length notes) 0)
                    (setq contact (cons (cons 'notes notes) contact))))
                (when addresses
                  (setq contact (cons (cons 'formatted-addresses addresses) contact)))
                (when phone-numbers
                  (setq contact (cons (cons 'phone-numbers phone-numbers) contact)))
                (when company
                  (setq contact (cons (cons 'company company) contact)))
                (when groups
                  (setq contact (cons (cons 'groups (list groups)) contact)))
                (when emails
                  (setq contact (cons (cons 'emails (list emails)) contact)))
                (when (and name (> (length name) 0))
                  (setq contact (cons (cons 'name name) contact)))
                
                (setq google-contacts-alist (cons contact google-contacts-alist)))))
      google-contacts-alist)))

(defun google-contacts-merge-with-bbdb(google-contacts)
  "Merges Google contacts in `google-contacts' to local BBDB address book."
  (while google-contacts
    (let* ((contact (car google-contacts))
           (emails (cadr (assoc 'emails contact)))
           (firstemail (car emails))
           (name (or (cdr (assoc 'name contact))
                     (substring firstemail 0 (string-match "@" firstemail))
                     firstemail))
           (aka (cdr (assoc 'aka contact)))
           (company (cdr (assoc 'company contact)))
           (birthday (cdr (assoc 'birthday contact)))
           (groups (cadr (assoc 'groups contact)))
           (phone-numbers-alist (cdr (assoc 'phone-numbers contact)))
           (addresses-alist (cdr (assoc 'formatted-addresses contact)))
           (contact-notes (cdr (assoc 'notes contact)))
           (case-fold-search t)
           (records (bbdb-records))
           notes phones addresses new-record)
      (while records
        ;; delete any matching records first
        (when (or (and name (bbdb-record-name (car records)) (string-match (concat "^" name "$") (bbdb-record-name (car records))))
                  (google-contacts-intersection-ignore-case emails (bbdb-record-net (car records))))
          (bbdb-delete-record-internal (car records)))
        (setq records (cdr records)))

      ;; Add any nick-name/AKA to mail alias/groups
      (when aka
        (setq groups (cons aka groups)))
      
      ;; Create BBDB notes field with mail-alias and possibly anniversary (birthday)
      (when (and groups emails) ;; Only populate mail-alias field if contact actually has email address(es).
        (setq notes (cons (cons 'mail-alias (mapconcat 'identity groups ", ")) notes)))

      (when contact-notes
        (setq notes (cons (cons 'notes contact-notes) notes)))

      (when phone-numbers-alist
        (setq phones (map 'list (lambda(phone)
                                  (vector (capitalize (symbol-name (car phone))) (cdr phone)))
                          phone-numbers-alist)))

      (when addresses-alist
        (setq addresses (map 'list (lambda(address)
                                     (vector (capitalize (symbol-name (car address))) (split-string (cdr address) "\n")
                                             "" "" "" ""))
                             addresses-alist)))
      
      (when birthday
        (setq notes (cons (cons 'anniversary (concat birthday " birthday")) notes)))
      
      ;; Create new record
      (message "creating new record for %s" name)
      (setq new-record (bbdb-create-internal name company (mapconcat 'identity emails ", ")
                                             addresses phones notes))
      ;; Add a real BBDB AKA field as well
      (when aka
        (bbdb-record-set-aka new-record (list aka))
        (bbdb-change-record new-record nil))
      )
      (setq google-contacts (cdr google-contacts))
    )
  (bbdb-resort-database)
  (bbdb-save-db)
  (message "Updated BBDB with Google contacts for %s" (car (google-contacts-get-credentials)))
  )

(defun google-contacts-update-bbdb()
  "Fetch contacts from Google and merge them into local BBDB."
  (interactive)
  (google-contacts-merge-with-bbdb (google-contacts-retrieve)))

(provide 'google-contacts)
