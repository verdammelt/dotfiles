(use-package cc-vars
  :ensure nil
  :init (setq-default c-default-style "linux"
                      c-basic-offset 4))

(use-package prog-mode
  :ensure nil
  :init (add-hook 'after-init-hook 'global-prettify-symbols-mode t)
  :config
  (progn
    (defun mjs/prog-fill-column ()
      (setq fill-column 80))
    (add-hook 'prog-mode-hook 'mjs/prog-fill-column)
    (add-hook 'prog-mode-hook 'display-line-numbers-mode)
    (add-hook 'prog-mode-hook 'whitespace-mode)))

(use-package scss-mode)

(use-package yaml-mode)

(use-package typescript-mode
  :mode ("\\.tsx\\'" . typescriptreact-mode)
  :config
  (define-derived-mode typescriptreact-mode typescript-mode "TSX")
  (defun mjs/diminish-typescript () (setq mode-name "TS"))
  (add-hook 'typescript-mode-hook #'mjs/diminish-typescript)
  (setq typescript-indent-level 4))

(use-package js
  :ensure nil
  :config
  (setq js-indent-level 2)
  (defun mjs/js-mode-setup ()
    (setq mode-name "JS"
          indent-tabs-mode nil))
  (add-hook 'js-mode-hook 'mjs/js-mode-setup))

(use-package ansi-color
  :ensure nil
  :commands (ansi-color-apply-on-region))

(use-package compile
  :ensure nil
  :defines compilation-error-regexp-alist compilation-error-regexp-alist-alist
  :config
  (progn
    (defun mjs/add-compilation-error-regexp (sym regexp file line col highlight)
      (add-to-list 'compilation-error-regexp-alist sym)
      (add-to-list 'compilation-error-regexp-alist-alist
                   (list sym regexp file line col highlight)))

    (setq compilation-scroll-output t)
    (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)
    (mjs/add-compilation-error-regexp
     'webpack-tsc-error-regexp "ERROR in \\(.*\\)(\\(.*\\),\\(.*\\))"
     1 2 3 1)
    (mjs/add-compilation-error-regexp
     'jest-error-stack "at .* (\\(.*\\):\\(.*\\):\\(.*\\))"
     1 2 3 1)
    (mjs/add-compilation-error-regexp
     'rspec "\\(rspec\\|#\\) \\(.*?\\):\\([0-9]+\\)"
     2 3 nil 1)))

(use-package eglot
  :hook ((typescript-mode . eglot-ensure)
         (typescriptreact-mode . eglot-ensure))
  :bind (("C-c e r" . #'eglot-rename)
         ("C-c e a" . #'eglot-code-actions))
  ;; need this because eglot clobbers flymake-diagnostic-functions
  :init (setq eglot-stay-out-of '(flymake))
  :config
  (defun mjs/add-eglot-flymake-backend ()
    (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t))
  (add-hook 'eglot-managed-mode-hook #'mjs/add-eglot-flymake-backend))

(use-package flymake
  :hook (ruby-mode)
  :bind (("C-c ! c" . #'flymake-start)
         ("C-c ! l" . #'flymake-show-buffer-diagnostics)
         ("C-c ! L" . #'flymake-show-project-diagnostics)
         ("M-n" . #'flymake-goto-next-error)
         ("M-p" . #'flymake-goto-prev-error)))

(use-package rbenv)

(use-package flymake-eslint
  :hook ((typescript-mode typescriptreact-mode) . mjs/flymake-eslint-enable)
  :init
  (defun mjs/flymake-eslint-enable ()
    "Enable flymake-eslint and set its project root directory if possible."
    (flymake-eslint-enable)
    (setq flymake-eslint-project-root
          (locate-dominating-file (buffer-file-name) ".eslintrc.js"))))

(use-package nvm
  :commands (nvm-use nvm-use-for nvm--installed-versions))

(use-package prettier-js
  :diminish (prettier-js-mode "Pr")
  :hook ((js-mode typescript-mode typescriptreact-mode web-mode) . prettier-js-mode))


(defun mjs/project-teardown-js-env ()
  (when mjs/current-project
    (let ((default-directory (project-root mjs/current-project)))
      (mjs/remove-node-modules-from-path)
      (mjs/remove-node-from-path))))

(defun mjs/project-setup-js-env ()
  (when mjs/current-project
    (let ((default-directory (project-root mjs/current-project)))
      (mjs/add-node-to-path)
      (mjs/add-node-modules-in-path))))

(add-hook 'mjs/project-teardown-hook 'mjs/project-teardown-js-env)
(add-hook 'mjs/project-setup-hook 'mjs/project-setup-js-env)

(define-auto-insert '("\\.tsx" . "TSX Component")
  '((read-string "Component name: " nil nil (file-name-base (buffer-file-name)))
    "import React from 'react';" \n \n
    "export const " str " = () => <div>blah</div>;" \n \n))

(define-auto-insert '("\\.test\\.tsx" . "TSX Component test")
  '((read-string "Component name: " nil nil (file-name-base (file-name-base (buffer-file-name))))
    "import { " str  " } from '../" str "';" \n \n
    "describe('" str "', () => {" \n
    > "test('fails', () => expect(true).toBe(false))" \n \n
    "})" \n))

(define-skeleton rspec-describe
  "Inserts a ruby rspec describe block"
  "What are you describing? "
  > "describe \"" str "\" do" \n
  _ \n
  "end" > )
(define-skeleton rspec-context
  "Inserts a ruby rspec context block"
  "What is the context? "
  > "context \"" str "\" do" \n
  - \n
  "end")
(define-skeleton rspec-it
  "Inserts an rspec it block"
  "It does what? "
  > "it \"" str "\" do" \n
  _ \n
  "end" > )
(define-skeleton rspec-subject
  "Inserts a subject block"
  ""
  > "subject { " - " }")
(define-skeleton rspec-let
  "Inserts a subject block"
  "variable: "
  > "let(:" str ") { " - " }")
(define-skeleton rspec-file
  "Inserts typical Rspec file structure"
  "Class/Module name: "
  "require \"spec_helper\"" \n
  \n
  "describe " str " do" \n
  > _ \n
  "end" > \n)

(use-package ruby-mode
  :ensure nil
  :config
  (quietly-read-abbrev-file)
  (define-auto-insert '("_spec\\.rb" . "RSpec File") 'rspec-file)
  (define-abbrev ruby-mode-abbrev-table "desc" "" 'rspec-describe)
  (define-abbrev ruby-mode-abbrev-table "cont" "" 'rspec-context)
  (define-abbrev ruby-mode-abbrev-table "test" "" 'rspec-it)
  (define-abbrev ruby-mode-abbrev-table "subj" "" 'rspec-subject)
  (define-abbrev ruby-mode-abbrev-table "let" "" 'rspec-let))

(define-skeleton js-fn
  "Insert a function stub"
  "Name: "
  > "function " str | (delete-backward-char 1) "() {" \n
  _ \n
  "}" >)

(define-skeleton jasmine-descr
  "Inserts jasmine describe block"
  "Describing what? "
  > "describe(\"" str "\", function() {" \n
  _ \n
  "})" >)

(define-skeleton jasmine-before
  "Inserts jasmine before block"
  (completing-read "scope: " '("All" "Each") nil t)
  > "before" str "(function() {" \n
  _ \n
  "})" > )

(define-skeleton jasmine-spec
  "Inserts a jasmine it block"
  "it does what? "
  > "it(\"" str "\", function() {" \n
  _ \n
  "})" > )
