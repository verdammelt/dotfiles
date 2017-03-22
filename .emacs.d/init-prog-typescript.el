(use-package typescript-mode
  :config
  (progn (setq typescript-indent-level 2)
         (add-hook 'typescript-mode-hook 'mjs/setup-tide)))

(use-package tide)

(defun mjs/setup-tide ()
  (tide-setup)
  (eldoc-mode +1))
