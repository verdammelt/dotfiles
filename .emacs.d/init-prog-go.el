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
  :hook (before-save-hook . gofmt-before-save)
  :bind ("M-." . godef-jump))

(use-package go-eldoc
  :hook (go-mode-hook . go-eldoc-setup))

(use-package company-go
  :hook (go-mode-hook . company-mode)
  :config (add-to-list 'company-backends 'company-go))

(use-package go-snippets)

(use-package go-guru
  :hook (go-mode-hook go-guru-hl-identifier-mode))
