;;;;
;;;; Lisp setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(defun mjs/setup-lispy-mode (hook)
  (mapcar (lambda (f) (add-hook hook f))
          '(paredit-mode rainbow-delimiters-mode turn-on-eldoc-mode)))

(setq-default slime-lisp-implementations
              '((sbcl ("~/.cim/bin/sbcl"))
                (clisp ("~/.cim/bin/clisp"))
                (ccl ("~/.cim/bin/ccl"))
                (ecl ("~/.cim/bin/ecl"))))

(with-eval-after-load "slime"
  (setq common-lisp-hyperspec-root
        "/usr/local/share/doc/hyperspec/HyperSpec/"
        common-lisp-hyperspec-symbol-table
        (concat common-lisp-hyperspec-root "Data/Map_Sym.txt")
        common-lisp-hyperspec-issuex-table
        (concat common-lisp-hyperspec-root "Data/Map_IssX.txt"))

  (defun mjs/string-starts-with (string prefix)
    "Return t if STRING starts with prefix."
    (string-match (rx-to-string `(: bos ,prefix) t)
                    string))

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

  (mapc (lambda (symbol)
          (advice-add symbol :around #'mjs/browse-file-url-with-eww))
        '(common-lisp-hyperspec
          common-lisp-hyperspec-format
          common-lisp-hyperspec-lookup-reader-macro))

  (mjs/setup-lispy-mode 'slime-repl-mode-hook))

(with-eval-after-load 'slime-company
  (setq slime-company-completion 'fuzzy))

(with-eval-after-load 'eldoc (diminish 'eldoc-mode))
(with-eval-after-load 'elisp-slime-nav (diminish 'elisp-slime-nav-mode))
(with-eval-after-load 'paredit (diminish 'paredit-mode))

(defun mjs/emacs-lisp-mode-setup () (setq mode-name "Elisp"))

(with-eval-after-load 'lisp-mode
  (mjs/setup-lispy-mode 'emacs-lisp-mode-hook)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-elisp-slime-nav-mode)
  (add-hook 'emacs-lisp-mode-hook 'mjs/emacs-lisp-mode-setup)

  (mjs/setup-lispy-mode 'lisp-mode-hook)
  (mjs/setup-lispy-mode 'lisp-interaction-mode-hook)

  (put 'define-test 'lisp-indent-function 1)

  (with-eval-after-load 'ielm
    (mjs/setup-lispy-mode 'ielm-mode-hook)
    (add-hook 'ielm-mode-hook 'turn-on-elisp-slime-nav-mode)))

(with-eval-after-load 'scheme
  (mjs/setup-lispy-mode 'scheme-mode-hook))

(defun mjs/clj-refactor-setup ()
  (setq mode-name "CLJ")
  (clj-refactor-mode)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (add-hook 'nrepl-connected-hook #'cljr-update-artifact-cache))

(with-eval-after-load 'clj-refactor (diminish 'clj-refactor-mode))

(with-eval-after-load 'clojure-mode
  (mjs/setup-lispy-mode 'clojure-mode-hook)
  (add-hook 'clojure-mode-hook 'mjs/clj-refactor-setup))

(with-eval-after-load 'cider-repl
  (mjs/setup-lispy-mode 'cider-repl-mode-hook)
  (cider-repl-add-shortcut "quit" 'cider-quit)
  (setq cider-repl-display-help-banner nil))

(with-eval-after-load 'simple
  (mjs/setup-lispy-mode 'eval-expression-minibuffer-setup-hook))
