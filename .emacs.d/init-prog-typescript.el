(declare-function projectile-project-root "projectile")
(defvar flycheck-typescript-tslint-config)
(defvar flycheck-typescript-tslint-executable)
(defun mjs/setup-tslint ()
  (let ((project-root (projectile-project-root)))
    (setq flycheck-typescript-tslint-config
          (expand-file-name "VestaWeb/tslint.json" project-root)
          flycheck-typescript-tslint-executable
          (expand-file-name "VestaWeb/node_modules/.bin/tslint" project-root))))

(defun mjs/setup-tide ()
  (tide-setup)
  (eldoc-mode +1))

(use-package typescript-mode
  :init (progn
          (add-hook 'typescript-mode-hook 'mjs/setup-tide)
          (add-hook 'projectile-after-switch-project-hook 'mjs/setup-tslint))
  :config
  (setq typescript-indent-level 4))

(use-package tide
  :config
  (setq tide-format-options
        '(:indentSize 4 :tabSize 4)))

(use-package web-mode
  :init (progn
          (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
          (add-hook 'web-mode-hook
                    (lambda ()
                      (when (string-equal "tsx" (file-name-extension buffer-file-name))
                        (mjs/setup-tide)))))
  :config
  (progn
    (flycheck-add-mode 'typescript-tslint 'web-mode)
    (advice-add 'web-mode-on-after-change :around #'mjs/fci-hack)
    (advice-add 'web-mode-on-post-command :around #'mjs/fci-hack)))
