;;;;
;;;; Lisp setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(setq slime-lisp-implementations
      '(("sbcl" ("sbcl"))
        ("cmucl" ("lisp"))))
(slime-setup '(slime-fancy))

(after "slime"
  (setq common-lisp-hyperspec-root
        "/usr/local/share/doc/hyperspec/HyperSpec/"
        common-lisp-hyperspec-symbol-table
        (concat common-lisp-hyperspec-root "Data/Map_Sym.txt")
        common-lisp-hyperspec-issuex-table
        (concat common-lisp-hyperspec-root "Data/Map_IssX.txt")))

(after 'eldoc (diminish 'eldoc-mode))
(after 'elisp-slime-nav (diminish 'elisp-slime-nav-mode))
(after 'paredit (diminish 'paredit-mode))

(defun mjs/emacs-lisp-mode-setup () (setq mode-name "Elisp"))

(after 'lisp-mode
  (if (or (fboundp 'paredit-mode)
          (autoloadp (symbol-function 'paredit-mode)))
      (add-hook 'emacs-lisp-mode-hook 'paredit-mode))
  (if (or (fboundp 'rainbow-delimiters-mode)
          (autoloadp (symbol-function 'rainbow-delimiters-mode)))
      (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
  (add-hook 'emacs-lisp-mode-hook 'mjs/emacs-lisp-mode-setup)

  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

  (after 'ielm
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-elisp-slime-nav-mode)))

(after 'scheme
  (add-hook 'scheme-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode))

(defun mjs/clj-refactor-setup ()
  (setq mode-name "CLJ")
  (clj-refactor-mode)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (add-hook 'nrepl-connected-hook #'cljr-update-artifact-cache))

(after 'clj-refactor (diminish 'clj-refactor-mode))

(after 'clojure-mode
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'clojure-mode-hook 'mjs/clj-refactor-setup)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode))

(after 'cider-repl
  (cider-repl-add-shortcut "quit" 'cider-quit))

(after 'simple
  (add-hook 'eval-expression-minibuffer-setup-hook 'paredit-mode))
