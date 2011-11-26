;;;
;;; set up lisp development environment
;;;
;;;; Modified Time-stamp: <2011-03-05 13:00:26 mark>
;; 
;; SLIME/Lisp
;;

(setq common-lisp-hyperspec-root
      "file:/opt/local/share/doc/lisp/HyperSpec-7-0/HyperSpec/") 

(defun lein-swank ()
  (interactive)
  (let ((root (locate-dominating-file default-directory "project.clj")))
    (or root (error "Not in a Leiningen project."))
    (shell-command (format "cd %s && lein swank %s &" root 4009)
                   "*lein-swank*")
    (set-process-filter (get-buffer-process "*lein-swank*")
                        ;; gAAAAAAAAAAAH! no leixcal scope? what is this, 1970?
                        (lambda (process output)
                          (when (string-match "Connection opened on" output)
                            (slime-connect "localhost" 4009)
                            (set-process-filter process nil))))
    (message "Starting swank server...")))


(provide 'init-lisp)
