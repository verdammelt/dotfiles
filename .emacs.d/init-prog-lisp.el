;;;;
;;;; Lisp setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(use-package slime
  :bind (("\C-cs" . slime-selector))
  :init
  (progn (setq-default slime-lisp-implementations
                       '((sbcl ("sbcl" "--noinform"))))
         (load (expand-file-name "~/.quicklisp/slime-helper.el")))
  :config
  (progn
    (load (expand-file-name "~/.quicklisp/clhs-use-local.el") t)
    (add-hook 'slime-repl-mode-hook 'paredit-mode)
    (slime-setup '(slime-fancy
                   slime-asdf
                   slime-quicklisp
                   slime-company))
    ;; this completion does not work well with capf as it opens its own buffer which is not what we want
    (remove-hook 'slime-completion-at-point-functions #'slime-c-p-c-completion-at-point)))

(use-package slime-company
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy))

(use-package slime-autodoc
  :ensure nil
  :config (setq slime-autodoc-mode-string nil))

(use-package eldoc
  :ensure nil
  :hook (after-init . global-eldoc-mode)
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
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
    (add-hook 'emacs-lisp-mode-hook 'mjs/emacs-lisp-mode-setup)

    (add-hook 'lisp-mode-hook 'paredit-mode)
    (add-hook 'lisp-interaction-mode-hook 'paredit-mode)

    (put 'define-test 'lisp-indent-function 1)
    (put 'test 'lisp-indent-function 1)

    (define-auto-insert '("\\.asd" . "ASDF Defsystem file") 'asdf-defsystem)))

(use-package ielm
  :ensure nil
  :config
  (progn
    (add-hook 'ielm-mode-hook 'paredit-mode)
    (add-hook 'ielm-mode-hook 'turn-on-elisp-slime-nav-mode)))

(use-package scheme
  :ensure nil
  :config (add-hook 'scheme-mode-hook 'paredit-mode))

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

(define-skeleton cl-defpacakge
  "Inserts a typical cl:defpackage form into the current buffer"
  "Package name: "
  "(defpackage #:" str \n
  > "(:use :cl)" \n
  > "(:export)"
  ")")
