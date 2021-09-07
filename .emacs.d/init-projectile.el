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

(use-package projectile
  :diminish projectile-mode
  :bind (("s-b" . projectile-switch-to-buffer)
         ("s-f" . projectile-find-file)
         ("s-s" . projectile-switch-project))
  :bind-keymap (("C-c p" . projectile-command-map))
  :functions (projectile-project-name)
  :init
  (progn (setq projectile-known-projects-file
               (locate-user-emacs-file ".projectile-bookmarks.eld")
               projectile-cache-file
               (locate-user-emacs-file ".projectile.cache")
               projectile-completion-system 'default)
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

    (setq projectile-switch-project-action 'projectile-dired
          projectile-enable-caching t)

    (projectile-register-project-type 'generic '() :compile "" :test "")

    (define-advice projectile-kill-buffers (:around (orig-fn prefix-arg) filter-switch)
         (interactive "P")
         (let ((projectile-kill-buffers-filter (if prefix-arg 'kill-all 'kill-only-files)))
           (call-interactively orig-fn)))

    (defvar mjs/default-projectile-indexing-method projectile-indexing-method)
    (defun mjs/setup-gtd-project-caching ()
      (let ((new-value (if (string= (projectile-project-name) "GTD")
                           'native
                         mjs/default-projectile-indexing-method)))
        (setq projectile-indexing-method new-value)))))
