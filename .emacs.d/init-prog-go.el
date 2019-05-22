;; some needed go tools:
;; 	go get github.com/rogpeppe/godef
;; 	go get -u github.com/mdempsky/gocode
;; 	go get -u golang.org/x/lint/golint
;; 	go get golang.org/x/tools/cmd/guru

(defvar *gopath* (expand-file-name "~/Documents/SRC/pax-go"))
(defvar *asdf-shims* (expand-file-name "~/.asdf/shims"))
(setenv "GOPATH" *gopath*)
(add-to-list 'exec-path *asdf-shims*)
(add-to-list 'exec-path (expand-file-name "bin" *gopath*))
(mjs/set-path-envvar-from-exec-path)

(use-package go-mode
  :hook (before-save . gofmt-before-save)
  :bind ("M-." . godef-jump)
  :config (setq-default tab-width 2))

(use-package protobuf-mode)

(use-package go-eldoc
  :hook (go-mode . go-eldoc-setup))

(use-package company-go
  :hook (go-mode . company-mode)
  :init (with-eval-after-load "company"
          (add-to-list 'company-backends 'company-go))
  :config (setq company-go-show-annotation t))

(use-package go-snippets)

(use-package go-guru
  :hook (go-mode . go-guru-hl-identifier-mode))
