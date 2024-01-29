;;; -*- lexical-binding: t -*-
;;;
;;; Modified version to fix regexp that finds error lines
;;;
;;; Original only recognized files ending in `.rb'
;;;
(defun ruby-flymake-rubocop (report-fn &rest _args)
  "RuboCop backend for Flymake."
  (unless (executable-find "rubocop")
    (error "Cannot find the rubocop executable"))

  (let ((command (list "rubocop" "--stdin" buffer-file-name "--format" "emacs"
                       "--cache" "false" ; Work around a bug in old version.
                       "--display-cop-names"))
        (default-directory default-directory)
        config-dir)
    (when buffer-file-name
      (setq config-dir (locate-dominating-file buffer-file-name
                                               ruby-rubocop-config))
      (if (not config-dir)
          (setq command (append command '("--lint")))
        (setq command (append command (list "--config"
                                            (expand-file-name ruby-rubocop-config
                                                              config-dir))))
        (when (ruby-flymake-rubocop--use-bundler-p config-dir)
          (setq command (append '("bundle" "exec") command))
          ;; In case of a project with multiple nested subprojects,
          ;; each one with a Gemfile.
          (setq default-directory config-dir)))

      (ruby-flymake--helper
       "rubocop-flymake"
       command
       (lambda (proc source)
         ;; Finding the executable is no guarantee of
         ;; rubocop working, especially in the presence
         ;; of rbenv shims (which cross ruby versions).
         (when (eq (process-exit-status proc) 127)
           ;; Not sure what to do in this case.  Maybe ideally we'd
           ;; switch back to ruby-flymake-simple.
           (flymake-log :warning "RuboCop returned status 127: %s"
                        (buffer-string)))
         (goto-char (point-min))
         (cl-loop
          while (search-forward-regexp
                 "^\\(?:.*\\|-\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\)$"
                 nil t)
          for msg = (match-string 3)
          for (beg . end) = (flymake-diag-region
                             source
                             (string-to-number (match-string 1))
                             (string-to-number (match-string 2)))
          for type = (cond
                      ((string-match "^[EF]: " msg)
                       :error)
                      ((string-match "^W: " msg)
                       :warning)
                      (t :note))
          collect (flymake-make-diagnostic source
                                           beg
                                           end
                                           type
                                           (substring msg 3))
          into diags
          finally (funcall report-fn diags)))))))
