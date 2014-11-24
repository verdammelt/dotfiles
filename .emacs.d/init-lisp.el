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

(after 'lisp-mode
  (if (or (fboundp 'paredit-mode)
	  (autoloadp (symbol-function 'paredit-mode)))
      (add-hook 'emacs-lisp-mode-hook 'paredit-mode))
  (if (or (fboundp 'rainbow-delimiters-mode)
	  (autoloadp (symbol-function 'rainbow-delimiters-mode)))
      (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)

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
  (clj-refactor-mode)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (add-hook 'nrepl-connected-hook #'cljr-update-artifact-cache))

(after 'clojure-mode
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook add-hook 'mjs/clj-refactor-setup)
  (add-hook add-hook 'rainbow-delimiters-mode))

(after 'cider-repl
  (cider-repl-add-shortcut "quit" 'cider-quit))
