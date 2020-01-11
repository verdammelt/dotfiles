;;;;
;;;; Lisp setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(declare-function 'subseq "cl-extra")

(defun mjs/setup-lispy-mode (hook)
  (mapcar (lambda (f) (add-hook hook f))
          '(paredit-mode rainbow-delimiters-mode turn-on-eldoc-mode)))

(use-package slime
  :init
  (progn (setq-default slime-lisp-implementations
                       '((sbcl ("sbcl" "--noinform"))))
         (load (expand-file-name "~/.quicklisp/slime-helper.el")))
  :config
  (progn
    (load (expand-file-name "~/.quicklisp/clhs-use-local.el") t)
    (mapc (lambda (symbol)
            (advice-add symbol :around #'mjs/browse-file-url-with-eww))
          '(common-lisp-hyperspec
            common-lisp-hyperspec-format
            common-lisp-hyperspec-lookup-reader-macro))

    (mjs/setup-lispy-mode 'slime-repl-mode-hook)

    (slime-setup '(slime-fancy slime-company)))
  :bind
  (("\C-cs" . slime-selector)))

(defun mjs/string-starts-with (string prefix)
  "Return t if STRING starts with prefix."
  (string-match (rx-to-string `(: bos ,prefix) t) string))

(defun mjs/browse-file-url-with-eww (next-method &rest args)
  (let ((browse-url-browser-function
         (lambda (file &optional new-window)
           (ignore new-window)
           (let ((file (if (mjs/string-starts-with file "file://")
                           (subseq file (length "file://"))
                         file)))
             (pop-to-buffer (get-buffer-create "*eww*"))
             (eww-open-file file)))))
    (apply next-method args)))

(use-package slime-company :config (setq slime-company-completion 'fuzzy))

(use-package eldoc
  :ensure nil
  :diminish (eldoc-mode)
  :config (eldoc-mode))
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

    (put 'define-test 'lisp-indent-function 1)))

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
