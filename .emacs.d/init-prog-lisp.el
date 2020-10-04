;;;;
;;;; Lisp setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(defun mjs/setup-lispy-mode (hook)
  (mapcar (lambda (f) (add-hook hook f))
          '(paredit-mode rainbow-delimiters-mode)))

(use-package slime
  :bind (("\C-cs" . slime-selector))
  :init
  (progn (setq-default slime-lisp-implementations
                       '((sbcl ("sbcl" "--noinform"))))
         (load (expand-file-name "~/.quicklisp/slime-helper.el")))
  :config
  (progn
    (load (expand-file-name "~/.quicklisp/clhs-use-local.el") t)
    (mjs/setup-lispy-mode 'slime-repl-mode-hook)

    (slime-setup '(slime-fancy slime-company))))

(use-package slime-autodoc
  :ensure nil
  :config (setq slime-autodoc-mode-string nil))

(use-package slime-company :config (setq slime-company-completion 'fuzzy))

(use-package eldoc
  :ensure nil
  :hook (after-init-hook . global-eldoc-mode)
  :diminish (eldoc-mode))
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
    (mjs/setup-lispy-mode 'emacs-lisp-mode-hook)
    (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
    (add-hook 'emacs-lisp-mode-hook 'mjs/emacs-lisp-mode-setup)

    (mjs/setup-lispy-mode 'lisp-mode-hook)
    (mjs/setup-lispy-mode 'lisp-interaction-mode-hook)

    (put 'define-test 'lisp-indent-function 1)

    (define-auto-insert '("\\.asd" . "ASDF Defsystem file") 'asdf-defsystem)))

(use-package ielm
  :ensure nil
  :config
  (progn
    (mjs/setup-lispy-mode 'ielm-mode-hook)
    (add-hook 'ielm-mode-hook 'turn-on-elisp-slime-nav-mode)))

(use-package scheme
  :ensure nil
  :config (mjs/setup-lispy-mode 'scheme-mode-hook))

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
    (mjs/setup-lispy-mode 'clojure-mode-hook)
    (add-hook 'clojure-mode-hook 'mjs/clj-refactor-setup)))

(use-package cider
  :commands (cider-repl-add-shortcut)
  :config
  (progn
    (mjs/setup-lispy-mode 'cider-repl-mode-hook)
    (add-hook 'cider-repl-mode-hook 'cider-company-enable-fuzzy-completion)
    (add-hook 'cider-mode-hook 'cider-company-enable-fuzzy-completion)
    (cider-repl-add-shortcut "quit" 'cider-quit)
    (cider-auto-test-mode 1)
    (set-face-foreground 'cider-fringe-good-face "lightgreen")))

(use-package simple
  :ensure nil
  :config (mjs/setup-lispy-mode 'eval-expression-minibuffer-setup-hook))

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

(define-skeleton cl-defpacakge
  "Inserts a typical cl:defpackage form into the current buffer"
  "Package name: "
  "(defpackage #:" str \n
  > "(:use :cl)" \n
  > "(:export)"
  ")")
