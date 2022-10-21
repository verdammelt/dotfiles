(use-package cc-vars
  :ensure nil
  :init (setq-default c-default-style "linux"
                      c-basic-offset 4))

;; editing programs
(defun mjs/prog-fill-column ()
  (setq fill-column 80))

(use-package prog-mode
  :ensure nil
  :init (add-hook 'after-init-hook 'global-prettify-symbols-mode t)
  :config (progn
            (add-hook 'prog-mode-hook 'mjs/prog-fill-column)
            (add-hook 'prog-mode-hook 'display-line-numbers-mode)
            (add-hook 'prog-mode-hook 'whitespace-mode)))

(use-package python
  :ensure nil
  :config (add-hook 'python-mode-hook #'(lambda () (setq fill-column 79))))
(use-package virtualenvwrapper
  :config (venv-initialize-interactive-shells))

(use-package ruby-mode
  :ensure nil
  :config (setq ruby-insert-encoding-magic-comment nil))

(use-package rbenv)

(use-package ansi-color
  :ensure nil
  :commands (ansi-color-apply-on-region))

(defvar compilation-filter-start)
(defun mjs/colorize-compilation ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(defun mjs/add-compilation-error-regexp (sym regexp file line col highlight)
  (add-to-list 'compilation-error-regexp-alist sym)
  (add-to-list 'compilation-error-regexp-alist-alist
               (list sym regexp file line col highlight)))

(use-package compile
  :ensure nil
  :defines compilation-error-regexp-alist compilation-error-regexp-alist-alist
  :config
  (progn (setq compilation-scroll-output 'first-error)
         (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
         (mjs/add-compilation-error-regexp
          'webpack-tsc-error-regexp "ERROR in \\(.*\\)(\\(.*\\),\\(.*\\))"
          1 2 3 1)
         (mjs/add-compilation-error-regexp
          'jest-error-stack "at .* (\\(.*\\):\\(.*\\):\\(.*\\))"
          1 2 3 1)
         (mjs/add-compilation-error-regexp
          'jest-error-stack-2 "at (\\(.*\\):\\(.*\\):\\(.*\\))"
          1 2 3 1)))

(use-package eglot
  :hook ((typescript-mode . eglot-ensure)
         (typescriptreact-mode . eglot-ensure))
  :bind (("C-c e r" . #'eglot-rename)
         ("C-c e a" . #'eglot-code-actions))
  ;; need this because eglot clobbers flymake-diagnostic-functions
  :init (setq eglot-stay-out-of '(flymake))
  :config (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t))

(use-package flymake
  :bind (("C-c ! c" . #'flymake-start)
         ("C-c ! l" . #'flymake-show-buffer-diagnostics)
         ("C-c ! L" . #'flymake-show-project-diagnostics)
         ("M-n" . #'flymake-goto-next-error)
         ("M-p" . #'flymake-goto-prev-error)))

(defun mjs/enable-flymake-eslint ()
  "Enable flymake-eslint and set its project root directory if possible."
  (flymake-eslint-enable)
  (setq flymake-eslint-project-root
        (locate-dominating-file (buffer-file-name) ".eslintrc.js")))

(use-package flymake-eslint
  :hook ((typescript-mode typescriptreact-mode) . mjs/enable-flymake-eslint))

(use-package scss-mode)

(use-package yaml-mode)

(provide 'init-prog)
