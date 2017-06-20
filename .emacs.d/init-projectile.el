(use-package projectile
  :config
  (progn
    (add-hook 'projectile-before-switch-project-hook 'mjs/remove-node-from-path)
    (add-hook 'projectile-after-switch-project-hook 'mjs/add-node-modules-in-path)
    (add-hook 'projectile-before-switch-project-hook 'mjs/remove-node-from-path)
    (add-hook 'projectile-after-switch-project-hook 'mjs/add-node-to-path)))

(setq projectile-known-projects-file
      (locate-user-emacs-file ".projectile-bookmarks.eld")
      projectile-cache-file
      (locate-user-emacs-file ".projectile.cache"))
(global-set-key (kbd "s-b") 'projectile-switch-to-buffer)
(global-set-key (kbd "s-f") 'projectile-find-file)
(global-set-key (kbd "s-s") 'projectile-switch-project)
(with-eval-after-load 'projectile
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  (setq projectile-mode-line
        '(:eval (propertize (format " :%s:" (projectile-project-name))
                            'face 'bold)))
  (setq mjs/default-projectile-indexing-method projectile-indexing-method)
  (defun mjs/setup-gtd-project-caching ()
    (let ((new-value (if (string= (projectile-project-name) "GTD")
                         'native
                       mjs/default-projectile-indexing-method)))
      (setq projectile-indexing-method new-value)))
  (add-hook 'projectile-after-switch-project-hook 'mjs/setup-gtd-project-caching)
  (add-hook 'projectile-after-switch-project-hook 'rvm-activate-corresponding-ruby)
  (add-hook 'projectile-mode-hook 'projectile-rails-on)
  (setq projectile-switch-project-action 'projectile-dired
        projectile-enable-caching t))
(projectile-mode)
