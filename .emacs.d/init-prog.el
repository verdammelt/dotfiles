(use-package cc-vars
  :ensure nil
  :init (setq c-default-style "linux"
              c-basic-offset 4))

;; editing programs
(defun mjs/prog-fill-column ()
  (setq fill-column 80))

(with-eval-after-load 'simple
  (add-hook 'prog-mode-hook 'mjs/prog-fill-column)
  (add-hook 'prog-mode-hook 'linum-mode)
  (add-hook 'prog-mode-hook 'fci-mode)
  (add-hook 'prog-mode-hook 'whitespace-mode))

(with-eval-after-load 'python
  (add-hook 'python-mode-hook #'(lambda () (setq fill-column 79))))

(with-eval-after-load 'ruby
  (setq ruby-insert-encoding-magic-comment nil))

(use-package rvm
  :defer 2
  :config (rvm-use-default))

(with-eval-after-load 'compile
  (defvar compilation-filter-start)
  (defvar compilation-scroll-output)

  (setq compilation-scroll-output 'first-error)

  (require 'ansi-color)

  (defun mjs/colorize-compilation ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point))))

  (add-hook 'compilation-filter-hook
            #'mjs/colorize-compilation))

(use-package less-css-mode)
