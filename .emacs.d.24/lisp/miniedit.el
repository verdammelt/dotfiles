;;; miniedit.el--- enhanced editing for minibuffer-fields.
;; Time-stamp: <2011-12-09 20:44:03 mark>
;; Copyright (C) 2001, 2002 Free Software Foundation, Inc.
;; Emacs Lisp Archive entry
;; Filename: miniedit.el
;; Package: miniedit
;; Author(s): Deepak Goel <deego@glue.umd.edu>,
;;            Christoph Conrad < christoph.conrad@gmx.de>
;; Version: 5.5alpha
;; Author's homepage: http://www.glue.umd.edu/~deego
;; For latest version:

(defvar miniedit-home-page
  "http://www.glue.umd.edu/~deego/emacspub/lisp-mine/miniedit/")
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
  "Drop file somewhere in your load-path, and add somewhere in your  .emacs.
 \(require 'miniedit\)
 \(miniedit-install\)

Xemacsers use \(miniedit-install-for-xemacs\) instead of
\(miniedit-install\).


Please type M-x miniedit-introduction, M-x miniedit-quick-start and
M-x miniedit-commentary for more details. "
)

;;;###autoload
(defun miniedit-quick-start ()
  "Provides electric help for function `miniedit-quick-start'."
  (interactive)
  (with-electric-help
   '(lambda () (insert miniedit-quick-start) nil) "*doc*"))

;;; Introduction:
;; Stuff that gets posted to gnu.emacs.sources
;; as introduction
(defvar miniedit-introduction
  "Adds a key \"C-M-e\" \(e for edit\) to the minibuffer-local-map, and
other similar maps, and binds it to the function miniedit.  This
means that when you are in a minibuffer, trying to enter something,
you can type C-M-e to go enter those fields in a nice full buffer
\(with text mode\) instead.  In particular, inserting new lines and
indenting is easy..  Helpful, for instance, when editing bbdb notes
fields, which tend to be multiline, \(right?\)

PS: Lots of code borrowed from checkdoc..
Comments, patches, and more features welcome :\)
 

Tested mainly on emacs21.  Now, it may work even on Xemacs for atleast
for some of the minibuffer-maps.

Please type M-x miniedit-introduction, M-x miniedit-quick-start and
M-x miniedit-commentary for more details. ")

;;;###autoload
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


New features of the past:
1.2 onwards:
Note that:
* Even though the full-buffer will be in text-mode, it will inherit
  all the keybindings of the relevant minibuffer, such as
  tab-completion etc.  These minibuffer keybindings can be accessed by
  first pressing a prefix-key C-p.  Anyone has any better ideas on
  this?

  I tried to make this C-c but somehow, that didn't work :\(  Any
  ideas or suggestions?



New features in version 1.0:
* A new co-author :\)
* the function miniedit is now available now in all sorts of minibuffers..
* more customizability: several hooks.. several more variables.
* Compatibility with emacs21..
* Thanks to Tom Fawcett, miniedit should now work with xemacs too.

"
)

;;;###autoload
(defun miniedit-commentary ()
  "Provides electric help for function `miniedit-commentary'."
  (interactive)
  (with-electric-help
   '(lambda () (insert miniedit-commentary) nil) "*doc*"))

;;; History:

;;; New features:
(defvar miniedit-new-features
  "
 ;;; 2002-06-28 T09:57:33-0400 (Friday)    D. Goel
 Will try to make sure that committing does not cause an error even if
 windows have been moved around before committing...


New in 1.3:
Minidedit tries to be responsible now.. does not alter
\(x\)emacs-defaults just because it has been loaded in.  It does its
frobbing only when asked to, via miniedit-install...

* Will NOT change the minibuffer-features unless asked to.
* Does NOT define any new functions like princ-list any more...
--takes care of the possibly missing xemacs feature by itself now.


* If requested, miniedit, when invoked, will add the contents of
minibuffer to kill-ring.  See variable `miniedit-before-edit-kill-p'.

* If requested, miniedit will add the contents of miniedit-buffer to
kill-ring before trying to commit the changes to the minibuffer.  Can
be turned off.  See `miniedit-after-edit-kill-p'.

The above two features are introduced because sometimes there are
errors when committing changes to the minibuffer, and adding stuff to
kill-ring might help save your changes.  Moreover, if you use Colin's
browse-kill-ring, using the kill-ring becomes much easier...


* Added customizability.. before editing or committing, allows you to
run some functions and hooks..

")

;;;###autoload
(defun miniedit-new-features ()
  "Provides electric help for function `miniedit-new-features'."
  (interactive)
  (with-electric-help
   '(lambda () (insert miniedit-new-features) nil) "*doc*"))

(defvar miniedit-version "5.5alpha")

;;; BUGS:
;;   Commit problem:  Once you are in the miniedit buffer, if you move
;;   buffers around, switch back and forth etc., the commit sometimes
;;   fails.. the author is working on this.... :)

;;==========================================
;;; code:
(eval-when-compile (require 'custom))
(eval-when-compile
  (require 'cl))

(defgroup miniedit nil
  "Miniedit"
  :group 'applications)

(defcustom miniedit-before-edit-kill-p nil
  "Add to kill ring before starting edit?"
  :group 'miniedit)

(defcustom miniedit-before-commit-kill-p nil
  "Add the string to `kill-ring' before committing?"
  :group 'miniedit
)

(defcustom miniedit-before-edit-function nil
 "Function to run on minibuffer-string before editing.

If this variable points to a function-name, that function is run on
the string that is gotten from the minibuffer..  The function should
do whatever it wants, and then it should return a (possibly) modified
string.  That modified string is what becomes the string to be
edited."

 :group 'miniedit
)

(defcustom miniedit-before-commit-function nil
 "If non-nil, function to run on minibuffer-string after editing.

If this variable points to a function-name, that function is run on
the string that is read from the miniedit-buffer..  The function should
do whatever it wants, and then it should return a (possibly) modified
string.  That modified string is what gets committed to the
minibuffer."
 :group 'miniedit

)


(defcustom miniedit-before-edit-hook nil
  "A hook thatis run before editing begins.."
  :group 'miniedit
)

(defcustom miniedit-before-commit-hook nil
  "A hook that is run before commitment to the minibuffer."
  :group 'miniedit
)

(defvar miniedit-string "miniedit-default-string"
  "This varible is what shall store the miniedit string temporarily...
This variable is introduced so that various miniedit-hooks can be used to
modify this string..")



(defmacro miniedit-withit (expr &rest rest)
  "Bind it to EXPR and do `REST'.

Caution: var-capture by its very nature.."
  `(let ((it ,expr))
     ,@rest))

;;;  Tom Fawcett <tfawcett@hpl.hp.com>
;; For us xemacs users who don't have princ-list
;(eval-when-compile
;  (unless (fboundp 'princ-list)
;    (defmacro princ-list (&rest things)
;      (cons 'progn (mapcar #'(lambda (x) `(princ ,x)) things)))))

;; special handling because princ-list is not defined for xemacs..

;;copied from mule-cmds.. and renamed...
(defun miniedit-princ-list (&rest args)
  "Same as `princ-list', but provided for Xemacs.  Print ARGS."
  (while args (princ (car args)) (setq args (cdr args)))
  (princ "\n"))

(defcustom miniedit-fill-column-deduction 14
  "The `fill-column' will be reduced from its default by this amount.

One would like this because part of the minibuffer is occupied by the
prompt string.  And, for instance, because in bbdb's notes, a large
left margin is taken up by the entry \"notes:\".

This variable can be assigned *anything* which results in an integer
when eval'ed."
  :group 'miniedit :type 'integer

)

 
;;;###autoload
(defun miniedit ()
  "The main function."
  (interactive)
  (let ((miniedit-string miniedit-string)
	(minibufname (buffer-name))
	)
    (save-excursion
      ;; so that it can be redefined below..
      (makunbound 'miniedit-mode-map)
      (easy-mmode-define-minor-mode
       miniedit-mode
       "The mode to inherit minibuffer keybindings"
       nil
       " MINI"
       ;; 3 means C-c
       ;; 16 means C-p
       (list 'keymap (cons 16 (current-local-map))))
      (define-key miniedit-mode-map "\C-c\C-c" 'exit-recursive-edit)
      (let ((contents
	     (miniedit-recursive-edit
	      ""
	      (progn
		(setq miniedit-string
		      (minibuffer-contents-no-properties))
		 (when (and
			(stringp miniedit-string)
			miniedit-before-edit-kill-p)
		   (kill-new miniedit-string))
		 (when
		     miniedit-before-edit-function
		   (miniedit-withit
		    (funcall miniedit-before-edit-function
			     miniedit-string)
		    (when it (setq miniedit-string it))))
		 (run-hooks 'miniedit-before-editing-hook)
		 miniedit-string)
	      
	      )))
	(delete-other-windows)
	(other-window 1)
	(miniedit-set-minibuffer-contents contents minibufname)
	))))
 
(defun miniedit-set-minibuffer-contents (contents minibuffer-name)
  "Set `minibuffer-contents' to CONTENTS.
The name of the minibuffer is MINIBUFFER-NAME.

version 21 or higher only.."
  (set-buffer minibuffer-name)
  (delete-minibuffer-contents)
  (insert contents))
 

;;;###autoload
(defun miniedit-install ()
  "Install miniedit by frobbing your miniedit-local maps."
  (interactive)
  (define-key minibuffer-local-map "\M-\C-e" 'miniedit)
  (define-key minibuffer-local-ns-map "\M-\C-e" 'miniedit)
  (define-key minibuffer-local-completion-map "\M-\C-e" 'miniedit)
  (define-key minibuffer-local-must-match-map "\M-\C-e" 'miniedit)
  )

;;; 2002-05-03 T20:51:31-0400 (Friday)    D. Goel
;;;###autoload
(defun miniedit-install-for-xemacs ()
  "Try to Install miniedit for Xemacs."
  (interactive)
  (ignore-errors (define-key minibuffer-local-map "\M-\C-e" 'miniedit))
  ;;(define-key minibuffer-local-ns-map "\M-\C-e" 'miniedit)
  (ignore-errors (define-key minibuffer-local-completion-map "\M-\C-e" 'miniedit))
  (ignore-errors (define-key minibuffer-local-must-match-map "\M-\C-e" 'miniedit))
  )

(defun miniedit-recursive-edit (msg &optional content)
  "Enter recursive edit to permit a user to edit long contents..
Useful when the original contents are in a minibuffer.  Transfer those
contents to a new buffer and edit them there.

MSG is a message, which is displayed in a Edit buffer.
Mostly copied from `checkdoc-recursive-edit'.
CONTENT is the content to be edited..
Then, returns a string...

Optimized for being called when the current buffer is a minibuffer.."
  (let ((this-buffer (buffer-name))
	(new-content content)
	save-content
	(errorp nil)
	)
    (save-excursion
      (other-window 1)
      (switch-to-buffer "*Miniedit*")
      (set-buffer "*Miniedit*")
      (setq save-content (buffer-substring (point-min) (point-max)))
      (delete-region (point-min) (point-max))
      (text-mode)
      (miniedit-mode t)
      (let ((fill-column (- fill-column
			    (eval miniedit-fill-column-deduction))))
	(if (stringp content) (insert content)
	  (setq errorp t))
	(unless errorp
	  (with-output-to-temp-buffer "*Miniedit Help*"
	    (miniedit-princ-list
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
		    ;;(delete-window (get-buffer-window "*Miniedit*"))
		    (kill-buffer "*Miniedit*")
		    )))
	    (kill-buffer "*Miniedit Help*"))))
      (unless (stringp new-content)
	(setq errorp t))

      
      ;;user-customization of new content begins..
      (setq miniedit-string
	    new-content)
      (when (and
	     (stringp miniedit-string)
	     miniedit-before-commit-kill-p)
	(kill-new miniedit-string))
      (when
	  miniedit-before-commit-function
	(miniedit-withit
	 (funcall miniedit-before-commit-function
		  miniedit-string)
	 (when it (setq miniedit-string it))))
      (run-hooks 'miniedit-before-committing-hook)
	;;user-customization of new content ends..
	

      (if (not errorp)
	  new-content
	save-content))))




(defun miniedit-recursive-edit-no-mini (msg &optional content)
  "No use of this function is currently known.
Enter recursive edit to permit a user to edit long bbdb contents..
MSG is a message, which is displayed in a Edit buffer.
Mostly copied from `checkdoc-recursive-edit'.
CONTENT is the content to be edited..
Then, returns a string...

Optimized for being called when the current buffer is not a minibuffer.."
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
	  (miniedit-princ-list
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
  


(provide 'miniedit)

;;; miniedit.el ends here
