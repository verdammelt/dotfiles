(declare-function projectile-project-root "projectile")
;; (defvar flycheck-tslint-args)
;; (defvar flycheck-typescript-tslint-config)
;; (defvar flycheck-typescript-tslint-executable)

(defun mjs/setup-tide ()
  (tide-setup)
  (eldoc-mode +1))

(use-package typescript-mode
  :hook (typescript-mode . mjs/setup-tide)
  :config
  (setq typescript-indent-level 2))

(use-package tide
  :config
  (setq tide-format-options
        '(:indentSize 2 :tabSize 2)))

(use-package web-mode
  :init (progn
          (setq standard-indent 2)
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
