;;;;
;;;; Lisp setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(use-package paredit
  :autoload (enable-paredit-mode)
  :diminish (paredit-mode)
  ;; changes to bindings of RET and C-j in v25+ causes problems with REPLs
  ;; this will change things back to the way it was.
  :bind ((:map paredit-mode-map
               (("RET" . nil)
                ("C-j" . paredit-newline))))
  :config
  (define-key paredit-mode-map (kbd "M-s") nil t)) ;; i like M-x o (occur) too much!

(use-package sly
  :pin melpa
  :hook ((sly-mode . (lambda () (unless (sly-connected-p) (save-excursion (sly)))))
         (sly-mrepl . enable-paredit-mode))
  :config
  (load "~/.quicklisp/clhs-use-local" :noerror)
  (keymap-global-set "C-c C-/" sly-selector-map))

(use-package sly-asdf :pin melpa)
(use-package sly-quicklisp :pin melpa
  :config (push (cons "quickload" #'sly-quickload) sly-mrepl-shortcut-alist))
(use-package sly-macrostep :pin melpa
  :bind ("C-c e" . #'macrostep-expand))

(use-package eldoc
  :ensure nil
  :hook (after-init . global-eldoc-mode)
  :diminish (eldoc-mode)
  :config (setq eldoc-echo-area-use-multiline-p t))

(use-package elisp-slime-nav
  :diminish (elisp-slime-nav-mode)
  :config (elisp-slime-nav-mode))

(use-package pp
  :ensure nil
  :bind (("C-x C-e" . pp-eval-last-sexp)
         ("M-:" . pp-eval-expression)))

(use-package elisp-mode
  :ensure nil
  :config
  (progn
    (defun mjs/emacs-lisp-mode-setup () (setq mode-name "Elisp"))
    (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
    (add-hook 'emacs-lisp-mode-hook 'mjs/emacs-lisp-mode-setup)
    (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)))

(use-package lisp-mode
  :ensure nil
  :config
  (progn
    (defun mjs/set-comment-start () (setq-local comment-start "; "))
    (setq inferior-lisp-program (executable-find "sbcl"))
    (add-hook 'lisp-data-mode-hook 'enable-paredit-mode)
    (add-hook 'lisp-data-mode-hook 'mjs/set-comment-start)

    (put 'define-test 'lisp-indent-function 1)
    (put 'test 'lisp-indent-function 1)

    (define-auto-insert '("\\.asd" . "ASDF Defsystem file") 'asdf-defsystem)
    (define-auto-insert '("packages?\\.lisp" . "Defpackage file") 'cl-defpackage)

    (cl-font-lock-built-in-mode)))

(use-package ielm
  :ensure nil
  :config
  (progn
    (add-hook 'ielm-mode-hook 'enable-paredit-mode)
    (add-hook 'ielm-mode-hook 'turn-on-elisp-slime-nav-mode)))

(use-package scheme
  :ensure nil
  :config (add-hook 'scheme-mode-hook 'enable-paredit-mode))

(use-package sicp)

(use-package simple
  :ensure nil
  :config (add-hook 'eval-expression-minibuffer-setup-hook 'enable-paredit-mode))

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

(define-skeleton aoc-day
  "Inserts a typical Advent-of-code day file template"
  ""
  "(defpackage #:aoc-" (setq year (skeleton-read "year: ")) "-" (setq day (skeleton-read "day: ")) \n
  > "(:use :cl))" \n \n
  > "(in-package #:aoc-" year "-" day ")" \n \n
  > "(aoc:def-today-suite*)" \n \n
  > "(defun read-data (file) (aoc:read-data file))" \n \n
  > "(defparameter +example+"\n
  > "(read-data (aoc:today-data-pathname \"example\")))" \n \n
  > "(defparameter +input+" \n "(read-data (aoc:today-data-pathname)))" \n \n
  > "(defun part1 (input) (declare (ignore input)) 0)" \n \n
  > "(5am:def-test part1 (:suite :aoc-" year "-" day ")"\n
  > "(5am:is (= -1 (part1 +example+)))" \n
  > "(5am:is (= -1 (part1 +input+))))" \n \n
  > "(defun part2 (input) (declare (ignore input)) 0)"\n \n
  > "(5am:def-test part2 (:suite :aoc-" year "-" day ")" \n
  > "(5am:skip \":aoc-" year "-" day ".2 not implemented\")" \n
  > ";; (5am:is (= -1 (part2 +example+)))" \n
  > ";; (5am:is (= -1 (part2 +input+)))" \n
  > ")")
