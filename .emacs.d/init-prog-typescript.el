(use-package typescript-mode
  :config (setq typescript-indent-level 4))

(use-package web-mode
  :init (progn
          (setq standard-indent 4)
          (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))
  :config
    (flycheck-add-mode 'typescript-tslint 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'web-mode))
