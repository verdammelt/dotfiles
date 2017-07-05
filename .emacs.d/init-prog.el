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
            (add-hook 'prog-mode-hook 'linum-mode)
            (add-hook 'prog-mode-hook 'fci-mode)
            (add-hook 'prog-mode-hook 'whitespace-mode)))

(use-package python
  :config (add-hook 'python-mode-hook #'(lambda () (setq fill-column 79))))
(use-package virtualenvwrapper
  :config (venv-initialize-interactive-shells))

(use-package ruby-mode
  :config (setq ruby-insert-encoding-magic-comment nil))

(use-package rvm
  :defer 2
  :config (rvm-use-default))

(use-package ansi-color
  :ensure nil
  :commands (ansi-color-apply-on-region))

(defvar compilation-filter-start)
(defun mjs/colorize-compilation ()
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(use-package compile
  :config
  (progn (setq compilation-scroll-output 'first-error)
         (add-hook 'compilation-filter-hook #'mjs/colorize-compilation)))

(use-package less-css-mode)
