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

    (add-hook 'projectile-before-switch-project-hook 'mjs/remove-node-from-path)
    (add-hook 'projectile-before-switch-project-hook 'mjs/remove-node-from-path)

    (add-hook 'projectile-after-switch-project-hook 'mjs/add-node-modules-in-path)
    (add-hook 'projectile-after-switch-project-hook 'mjs/add-node-to-path)
    (add-hook 'projectile-after-switch-project-hook 'mjs/setup-gtd-project-caching)
    (add-hook 'projectile-after-switch-project-hook 'rvm-activate-corresponding-ruby)

    (add-hook 'projectile-mode-hook 'projectile-rails-on)

    (setq projectile-switch-project-action 'projectile-dired
          projectile-enable-caching t)

    (projectile-register-project-type 'generic '() :compile "" :test "")

    (defvar mjs/default-projectile-indexing-method projectile-indexing-method)
    (defun mjs/setup-gtd-project-caching ()
      (let ((new-value (if (string= (projectile-project-name) "GTD")
                           'native
                         mjs/default-projectile-indexing-method)))
        (setq projectile-indexing-method new-value)))))
