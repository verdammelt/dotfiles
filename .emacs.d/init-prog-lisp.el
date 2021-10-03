;;;;
;;;; Lisp setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(use-package sly
  :pin melpa
  :hook ((sly-mode . (lambda () (unless (sly-connected-p) (save-excursion (sly)))))
         (sly-mrepl . paredit-mode))
  :config (setq common-lisp-hyperspec-root
                "file:///usr/local/Cellar/hyperspec/7.0/share/doc/hyperspec/HyperSpec/"))

(use-package sly-asdf :pin melpa)
(use-package sly-quicklisp :pin melpa)

(use-package eldoc
  :ensure nil
  :hook (after-init . global-eldoc-mode)
  :diminish (eldoc-mode)
  :config (setq eldoc-echo-area-use-multiline-p t))
(use-package elisp-slime-nav
  :diminish (elisp-slime-nav-mode)
  :config (elisp-slime-nav-mode))
(use-package paredit
  :diminish (paredit-mode)
  :config (paredit-mode))

(defun mjs/emacs-lisp-mode-setup () (setq mode-name "Elisp"))

(use-package lisp-mode
  :ensure nil
  :config
  (progn
    (setq inferior-lisp-program "/usr/local/bin/sbcl")
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
    (add-hook 'emacs-lisp-mode-hook 'mjs/emacs-lisp-mode-setup)

    (add-hook 'lisp-mode-hook 'paredit-mode)
    (add-hook 'lisp-interaction-mode-hook 'paredit-mode)

    (put 'define-test 'lisp-indent-function 1)
    (put 'test 'lisp-indent-function 1)

    (define-auto-insert '("\\.asd" . "ASDF Defsystem file") 'asdf-defsystem)
    (define-auto-insert '("packages?\\.lisp" . "Defpackage file") 'cl-defpackage)

    (unless (mjs/emacs-27-p)
      (cl-font-lock-built-in-mode))))

(use-package ielm
  :ensure nil
  :config
  (progn
    (add-hook 'ielm-mode-hook 'paredit-mode)
    (add-hook 'ielm-mode-hook 'turn-on-elisp-slime-nav-mode)))

(use-package scheme
  :ensure nil
  :config (add-hook 'scheme-mode-hook 'paredit-mode))

(use-package sicp)

(defun mjs/clj-refactor-setup ()
  (setq mode-name "CLJ")
  (clj-refactor-mode)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (add-hook 'nrepl-connected-hook #'cljr-update-artifact-cache))

(use-package clj-refactor
  :commands (cljr-update-artifact-cache)
  :diminish (clj-refactor-mode))

(use-package clojure-mode
  :config
  (progn
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (add-hook 'clojure-mode-hook 'mjs/clj-refactor-setup)))

(use-package cider
  :commands (cider-repl-add-shortcut)
  :config
  (progn
    (add-hook 'cider-repl-mode-hook 'paredit-mode)
    (add-hook 'cider-repl-mode-hook 'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-mode-hook 'cider-company-enable-fuzzy-completion)
    (cider-repl-add-shortcut "quit" 'cider-quit)
    (cider-auto-test-mode 1)
    (set-face-foreground 'cider-fringe-good-face "lightgreen")))

(use-package simple
  :ensure nil
  :config (add-hook 'eval-expression-minibuffer-setup-hook 'paredit-mode))

(define-skeleton asdf-defsystem
  "Inserts a typical ASDF defsystem form into the current buffer"
  "System Name: "
  "(defsystem \"" str | (file-name-base (buffer-file-name)) "\"" \n
  > ":description \"\"" \n
  > ":version \"0.0.0\"" \n
  > ":author \"" user-full-name "\"" \n
  > ":mailto \"" user-mail-address "\"" \n
  > \n
  > ":depends-on ()" \n
  > \n
  > ":pathname \"\"" \n
  > ":serial t" \n
  > ":components ((:file \"packages\"))"
  ")")

(define-skeleton cl-defpackage
  "Inserts a typical cl:defpackage form into the current buffer"
  "Package name: "
  "(defpackage :" str \n
  > "(:use :cl)" \n
  > "(:export)"
  ")")
