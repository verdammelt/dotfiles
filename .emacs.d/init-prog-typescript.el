(use-package typescript-mode
  :hook ((typescript-mode . tide-setup))
  :config
  (setq typescript-indent-level 4))

(use-package tide
  :config
  (setq tide-format-options
        '(:indentSize 4 :tabSize 4))
  (flycheck-add-next-checker 'typescript-tide '(warning . javascript-eslint) 'append)
  (flycheck-add-next-checker 'tsx-tide '(warning . javascript-eslint) 'append))

(use-package web-mode
  :init (progn
          (setq standard-indent 4)
          (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
          (add-hook 'web-mode-hook
                    (lambda ()
                      (when (string-equal "tsx" (file-name-extension buffer-file-name))
                        (tide-setup)))))
  :config
    (flycheck-add-mode 'typescript-tslint 'web-mode)
    (flycheck-add-mode 'javascript-eslint 'web-mode))
