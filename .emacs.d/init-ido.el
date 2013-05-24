;;;;
;;;; IDO
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Time-stamp: <2013-05-24 18:21:26 mark>
;;;;
(after 'ido
  (setq ido-show-dot-for-dired t
	ido-enable-flex-matching t)

					; sort ido filelist by mtime instead of alphabetically
  (add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
  (add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
  (defun ido-sort-mtime ()
    (setq ido-temp-list
	  (sort ido-temp-list 
		(lambda (a b)
		  (time-less-p
		   (sixth (file-attributes (concat ido-current-directory b)))
		   (sixth (file-attributes (concat ido-current-directory a)))))))
    (ido-to-end  ;; move . files to end (again)
     (delq nil (mapcar
		(lambda (x) (and (char-equal (string-to-char x) ?.) x))
		ido-temp-list))))

  ;; Display ido results vertically, rather than horizontally
  (setq ido-decorations 
      '("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" 
	" [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))
  (defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
  (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation))

(provide 'init-ido)
