(defun mjs/teardown-project ()
  (let ((teardown-symbol (intern (format "teardown-%s"
                                         (projectile-project-name)))))
    (when (symbol-function teardown-symbol)
      (message "Calling project teardown: %s" (symbol-name teardown-symbol))
      (funcall teardown-symbol))))

(defun mjs/setup-project ()
  (let ((setup-symbol (intern (format "setup-%s"
                                      (projectile-project-name)))))
    (message "project setup hook")
    (when (symbol-function setup-symbol)
      (message "Calling project setup: %s" (symbol-name setup-symbol))
      (funcall setup-symbol))))

(defvar *pax-ignored-directories*
  '("out" "webapp/node_modules" "webapp/build"))

(defun setup-pax ()
  (let ((asdf-dir "/usr/local/opt/asdf"))
    (setenv "ASDF_DIR" asdf-dir)
    (pushnew (expand-file-name "bin" asdf-dir) exec-path)
    (pushnew (expand-file-name "shims" asdf-dir) exec-path)
    (pushnew (expand-file-name "~/.asdf/.shims") exec-path)
    (mjs/set-path-envvar-from-exec-path)

    (dolist (p *pax-ignored-directories*)
      (pushnew p projectile-globally-ignored-directories :test #'string=))))

(defun teardown-pax ()
  (let ((asdf-dir (getenv "ASDF_DIR")))
    (setenv "ASDF_DIR")
    (setf exec-path (cl-remove (expand-file-name "bin" asdf-dir) exec-path)
          exec-path (cl-remove (expand-file-name "shims" asdf-dir) exec-path)
          exec-path (cl-remove (expand-file-name "~/.asdf/.shims") exec-path))
    (mjs/set-path-envvar-from-exec-path)
    (dolist (d *pax-ignored-directories*)
      (setf projectile-globally-ignored-directories
            (cl-remove d projectile-globally-ignored-directories :test #'string=)))))

(use-package projectile
  :bind (("s-b" . projectile-switch-to-buffer)
         ("s-f" . projectile-find-file)
         ("s-s" . projectile-switch-project))
  :bind-keymap (("C-c p" . projectile-command-map))
  :functions (projectile-project-name)
  :init
  (progn (setq projectile-known-projects-file
               (locate-user-emacs-file ".projectile-bookmarks.eld")
               projectile-cache-file
               (locate-user-emacs-file ".projectile.cache"))
         (add-hook 'after-init-hook 'projectile-mode t))

  :config
  (progn
    (add-to-list 'projectile-globally-ignored-directories "node_modules")

    (add-hook 'projectile-before-switch-project-hook 'mjs/teardown-project)
    (add-hook 'projectile-before-switch-project-hook 'mjs/remove-node-from-path)
    (add-hook 'projectile-before-switch-project-hook 'mjs/remove-node-from-path)

    (add-hook 'projectile-after-switch-project-hook 'mjs/add-node-modules-in-path)
    (add-hook 'projectile-after-switch-project-hook 'mjs/add-node-to-path)
    (add-hook 'projectile-after-switch-project-hook 'mjs/setup-gtd-project-caching)
    (add-hook 'projectile-after-switch-project-hook 'rvm-activate-corresponding-ruby)
    (add-hook 'projectile-after-switch-project-hook 'mjs/setup-project)

    (add-hook 'projectile-mode-hook 'projectile-rails-on)

    (setq projectile-switch-project-action 'projectile-dired
          projectile-mode-line-prefix " P"
          projectile-enable-caching t)

    (projectile-register-project-type 'generic '() :compile "" :test "")

    (defvar mjs/default-projectile-indexing-method projectile-indexing-method)
    (defun mjs/setup-gtd-project-caching ()
      (let ((new-value (if (string= (projectile-project-name) "GTD")
                           'native
                         mjs/default-projectile-indexing-method)))
        (setq projectile-indexing-method new-value)))))
