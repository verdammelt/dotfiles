(use-package projectile
  :config
  (progn
    (add-hook 'projectile-before-switch-project-hook 'mjs/remove-node-from-path)
    (add-hook 'projectile-after-switch-project-hook 'mjs/add-node-modules-in-path)
    (add-hook 'projectile-before-switch-project-hook 'mjs/remove-node-from-path)
    (add-hook 'projectile-after-switch-project-hook 'mjs/add-node-to-path)))
