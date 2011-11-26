;; miniedit.el --- enhanced editing for minibuffer-fields.
;; Time-stamp: <2006-04-23 19:35:58 damned>
;; Copyright (C) Deepak Goel 2001
;; Emacs Lisp Archive entry
;; Filename: miniedit.el
;; Package: miniedit
;; Author(s): Deepak Goel <d...@glue.umd.edu>,
;;            Christoph Conrad <c...@cli.de>
;; Version: 1.0
;; Author's homepage: http://www.glue.umd.edu/~deego
;; For latest version:

(defvar miniedit-home-page  
  "http://www.glue.umd.edu/~deego/emacspub/miniedit/")

;; This file is NOT (yet) part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; See also:

;; Quick start:
(defvar miniedit-quick-start
  "Insert somewhere in your load-path, and add
 \(require 'miniedit\) in your  .emacs.
"
)

(defun miniedit-quick-start ()
  "Provides electric help for function `miniedit-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert miniedit-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar miniedit-introduction
  "hi all,

The earlier file i posted bbdbedit.el, is now irrelevant.  Use this
miniedit.el instead.

Adds a key \"C-M-e\" \(e for edit\) to the minibuffer-local-map, and
other similar maps, and binds it to the function miniedit.  This
means that when you are in a minibuffer, trying to enter something,
you can type C-M-e to go enter those fields in a nice full buffer
\(with text mode\) instead.  In particular, inserting new lines and
indenting is easy..  Helpful, for instance, when editing bbdb notes
fields, which tend to be multiline, \(right?\)

PS: Lots of code borrowed from checkdoc..
Comments, patches, and more features welcome :\)

Tested only on GNU emacs 20.3 on *nix, and on emacs21.
Should work with xemacs.")

(defun miniedit-introduction ()
  "Provides electric help for function `miniedit-introduction'."
  (interactive)
  (with-electric-help
   '(lambda () (insert miniedit-introduction) nil) "*doc*"))

;;; Commentary:
(defvar miniedit-commentary
  "Type M-x miniedit-introduction.
   Hint to remembering keys:
I think of C-M-e as editg and C-M-c as commit.. any others?
"
)

(defun miniedit-commentary ()
  "Provides electric help for function `miniedit-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert miniedit-commentary) nil) "*doc*"))

;;; History:

;;; New features:
(defvar miniedit-new-features
  "New features in version 1.0:

* A new co-author :)

* the function miniedit is now available now in all sorts of minibuffers..

* more customizability: several hooks.. several more variables.

* Compatibility with emacs21..  

* Thanks to Tom Fawcett, miniedit should now work with xemacs too.")

(defun miniedit-new-features ()
  "Provides electric help for function `miniedit-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert miniedit-new-features) nil) "*doc*"))

(defvar miniedit-version "1.0")

;;; BUGS:
;; Does not yet work with C-s.  Any help welcome...

;;==========================================
;;; code:

(defvar miniedit-before-load-hook nil)
(defvar miniedit-after-load-hook nil)
(run-hooks 'miniedit-before-load-hook)

;;;  Tom Fawcett <tfawc...@hpl.hp.com>  
;; For us xemacs users who don't have princ-list
(eval-when-compile
  (unless (fboundp 'princ-list)
    (defmacro princ-list (&rest things)
      (cons 'progn (mapcar #'(lambda (x) `(princ ,x)) things)))))

(defvar miniedit-fill-column-deduction 14
  "The fill-column will be reduced from its default by this amount.

One would like this because part of the minibuffer is occupied by the
prompt string.  And, for instance, because in bbdb's notes, a large
left margin is taken up by the entry \"notes:\".

This variable can be assigned *anything* which results in an integer
when eval'ed."
)

(defun miniedit-minibuffer-contents ()
  "Should work for both emacs 20 and 21..
Assumes that we are already in the minibuffer.
"
  (if (< emacs-major-version 21)
      (buffer-substring (point-min) (point-max))
    (minibuffer-contents)))

(defun miniedit ()
  "The main function"
  (interactive)
  (save-excursion
    (let ((contents
           (miniedit-recursive-edit
            "" (miniedit-minibuffer-contents))))
      (delete-other-windows)
      (other-window 1)
      (miniedit-insert-minibuffer-contents contents)
      )))

(defun miniedit-insert-minibuffer-contents (contents)
  "Should work for both emacs20 and emacs 21..
Assumes we are in the minibuffer..
"
  (if (< emacs-major-version 21)
      (kill-region (point-min) (point-max))
    (delete-minibuffer-contents))
  (insert contents))

(define-key minibuffer-local-map "\M-\C-e" 'miniedit)
(define-key minibuffer-local-ns-map "\M-\C-e" 'miniedit)
(define-key minibuffer-local-completion-map "\M-\C-e" 'miniedit)
(define-key minibuffer-local-must-match-map "\M-\C-e" 'miniedit)

(defun miniedit-recursive-edit (msg &optional content)
  "Enter recursive edit to permit a user to edit long contents.. Useful
when the original contents are in a minibuffer.  Transfer those
contents to a new buffer and edit them there.

MSG is a message, which is displayed in a Edit buffer.
Mostly copied from `checkdoc-recursive-edit'.
CONTENT is the content to be edited..
Then, returns a string...

Optimized for being called when the current buffer is a minibuffer..
"
  (let ((this-buffer (buffer-name))
        (new-content content)
        )
    (save-excursion
      (other-window 1)
      (switch-to-buffer "*Miniedit*")
      (set-buffer "*Miniedit*")
      (kill-region (point-min) (point-max))
      (text-mode)
      (let ((fill-column (- fill-column
                            (eval miniedit-fill-column-deduction))))
        (if (stringp content) (insert content))
        (with-output-to-temp-buffer "*Miniedit Help*"
          (princ-list
           "Read THIS MESSAGE --->\n  " msg
           "\n\nEdit field, and press C-M-c to continue."))
        (shrink-window-if-larger-than-buffer
         (get-buffer-window "*Miniedit Help*"))
        (message "When you're done editing press C-M-c to continue.")
        (unwind-protect
            (recursive-edit)
          (if (get-buffer-window "*Miniedit*")
              (progn
                (progn
                  (setq new-content (buffer-substring
                                     (point-min) (point-max)))
                  (delete-window (get-buffer-window "*Miniedit*"))
                  (kill-buffer "*Miniedit*")
                  )))
          (kill-buffer "*Miniedit Help*")))
      new-content)))

(defun miniedit-recursive-edit-no-mini (msg &optional content)
  "No use of this function is currently known.
Enter recursive edit to permit a user to edit long bbdb contents..
MSG is a message, which is displayed in a Edit buffer.
Mostly copied from `checkdoc-recursive-edit'.
CONTENT is the content to be edited..
Then, returns a string...

Optimized for being called when the current buffer is not a minibuffer..
"
  (let ((this-buffer (buffer-name))
        (new-content content)
        )
    (save-excursion
      ;(other-window 1)
      (switch-to-buffer "*Miniedit*")
      (set-buffer "*Miniedit*")
      (kill-region (point-min) (point-max))
      (text-mode)
      (let ((fill-column (- fill-column 16)))
        (if (stringp content) (insert content))
        (with-output-to-temp-buffer "*Miniedit Help*"
          (princ-list
           "IMPORTANT: Read THIS MESSAGE --->\n  " msg
           "\n\nEdit field, and press C-M-c to continue."))
        (shrink-window-if-larger-than-buffer
         (get-buffer-window "*Miniedit Help*"))
        (message "When you're done editing press C-M-c to continue.")
        (unwind-protect
            (recursive-edit)
          (if (get-buffer-window "*Miniedit*")
              (progn
                (progn
                  (setq new-content (buffer-substring
                                     (point-min) (point-max)))
                  (delete-window (get-buffer-window "*Miniedit*"))
                  (kill-buffer "*Miniedit*")
                  )))
          (kill-buffer "*Miniedit Help*")))
      (switch-to-buffer this-buffer)
      new-content)))

(run-hooks 'miniedit-after-load-hook)

(provide 'miniedit)

;;; miniedit.el ends here 