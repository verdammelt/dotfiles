;;;;
;;;; Lisp setup
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(setq-default slime-lisp-implementations
              '((sbcl ("~/.cim/bin/sbcl"))
                (clisp ("~/.cim/bin/clisp"))
                (ccl ("~/.cim/bin/ccl"))
                (ecl ("~/.cim/bin/ecl"))))

(slime-setup '(slime-fancy))

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

  (advice-add 'common-lisp-hyperspec :around #'mjs/browse-file-url-with-eww)


  (add-hook 'slime-mode-hook (lambda () (set-up-slime-ac t)))
  (add-hook 'slime-repl-mode-hook (lambda () (set-up-slime-ac t)))
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'slime-repl-mode)))

(with-eval-after-load 'eldoc (diminish 'eldoc-mode))
(with-eval-after-load 'elisp-slime-nav (diminish 'elisp-slime-nav-mode))
(with-eval-after-load 'paredit (diminish 'paredit-mode))

(defun mjs/emacs-lisp-mode-setup () (setq mode-name "Elisp"))

(with-eval-after-load 'lisp-mode
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

  (put 'define-test 'lisp-indent-function 1)

  (with-eval-after-load 'ielm
    (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
    (add-hook 'ielm-mode-hook 'turn-on-elisp-slime-nav-mode)))

(with-eval-after-load 'scheme
  (add-hook 'scheme-mode-hook 'paredit-mode)
  (add-hook 'scheme-mode-hook 'rainbow-delimiters-mode))

(defun mjs/clj-refactor-setup ()
  (setq mode-name "CLJ")
  (clj-refactor-mode)
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (add-hook 'nrepl-connected-hook #'cljr-update-artifact-cache))

(with-eval-after-load 'clj-refactor (diminish 'clj-refactor-mode))

(with-eval-after-load 'clojure-mode
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'clojure-mode-hook 'mjs/clj-refactor-setup)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode))

(with-eval-after-load 'cider-repl
  (cider-repl-add-shortcut "quit" 'cider-quit)
  (setq cider-repl-display-help-banner nil))

(with-eval-after-load 'simple
  (add-hook 'eval-expression-minibuffer-setup-hook 'paredit-mode))
