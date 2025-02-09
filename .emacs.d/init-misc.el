;;;;
;;;; MISC
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;; Save my place in files
(use-package saveplace
  :ensure nil
  :init (add-hook 'after-init-hook 'save-place-mode t)
  :config
  (setq save-place-file (locate-user-emacs-file ".places")))

;; Save minibuffer history
(use-package savehist
  :ensure nil
  :init (add-hook 'after-init-hook 'savehist-mode t)
  :config
  (setq savehist-file (locate-user-emacs-file ".history")))

(use-package ps-print
  :ensure nil
  :config
  (setq
   ps-lpr-command (expand-file-name "~/bin/psprint")
   ps-spool-duplex t))

;; Backup files
(use-package files
  :ensure nil
  :init
  (progn (add-hook 'before-save-hook 'time-stamp)
         (add-hook 'after-save-hook
                   'executable-make-buffer-file-executable-if-script-p)
         (setq confirm-kill-emacs 'yes-or-no-p
               version-control t
               delete-old-versions t
               backup-by-copying-when-linked t
               backup-directory-alist
               (cl-acons "." (locate-user-emacs-file ".backups") nil)
               delete-by-moving-to-trash t
               trash-directory (expand-file-name "~/.Trash"))
         (setq auto-save-visited-predicate (lambda () (eq major-mode 'org-mode) ))
         (auto-save-visited-mode)))

(use-package paragraphs
  :ensure nil
  :init (setq sentence-end-double-space nil))

(use-package battery
  :ensure nil
  :init
  (progn
    (setq battery-mode-line-format "[%b%p%% %t] ")
    (display-battery-mode)))

(use-package abbrev
  :ensure nil
  :diminish abbrev-mode
  :init (setq-default abbrev-mode t)
  :config
  (setq abbrev-suggest t))

(use-package flyspell
  :ensure nil
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :diminish flyspell-mode
  :config
  (progn
    (setq flyspell-use-meta-tab nil
          flyspell-abbrev-p t)))

(use-package fullframe)

(use-package magit
  :pin melpa
  :bind (("C-x g" . magit-status)
         (:map magit-status-mode-map
               ("C-c r" . #'code-review-forge-pr-at-point)))
  :functions (magit-mode-quit-window)
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1
        magit-diff-refine-hunk 'all))

(defun mjs/forge-browse (arg)
  (interactive "p")
  (if (= arg 1)
      (forge-browse)
    (if-let ((target (forge--browse-target)))
        (let ((url (if (stringp target) target (forge-get-url target))))
          (kill-new url)
          (message url))
      (user-error "Nothing to browse here"))))

(use-package forge
  :pin melpa
  :after magit
  :demand t
  :config
  (setq forge-status-buffer-default-topic-filters
        (forge--topics-spec :type 'topic :active t :state 'open
                            :limit 25 :order 'recently-updated))
  (keymap-set magit-mode-map "<remap> <magit-browse-thing>" #'mjs/forge-browse))

(use-package code-review
  :demand t
  :vc (:url "https://github.com/phelrine/code-review.git"
            :branch "fix/closql-update"))

(use-package emojify
  :hook (code-review-mode-hook . #'emojify-mode))

(use-package magit-todos
  :after magit
  :demand t
  :config
  (magit-todos-mode)
  (setq magit-todos-rg-extra-args '("--hidden")
        magit-todos-exclude-globs '("rr-cache")))

(use-package hl-todo
  :init (global-hl-todo-mode))

(use-package simple
  :ensure nil
  :hook (markdown-mode . visual-line-mode)
  :init
  (setq-default indent-tabs-mode nil)
  (setq
   kill-whole-line t
   shell-command-prompt-show-cwd t
   visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))

(use-package whitespace
  :ensure nil
  :diminish whitespace-mode
  :config
  (setq whitespace-style '(face indentation empty trailing)
        whitespace-action '(auto-cleanup warn-if-read-only)))

(use-package ns-win
  :ensure nil
  :init
  (setq ns-use-srgb-colorspace t
        mac-command-modifier 'meta
        mac-option-modifier 'super
        mac-function-modifier 'hyper))

;; Misc
(use-package gnutls
  :ensure nil
  :init (setq gnutls-min-prime-bits 1024))

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(setq use-short-answers t)

(defun mjs/change-size (size)
  (interactive "nsize: ")
  (if (< size 100) (setq size (* 10 size)))
  (dolist (face '(default fixed-pitch variable-pitch))
    (set-face-attribute face nil :height size)))
(global-set-key (kbd "H-s") 'mjs/change-size)

;; (use-package expand-region
;;   :bind (("C-=" . er/expand-region)))

(use-package find-func
  :ensure nil
  :bind (:map help-map
              ("C-l" . find-library)
              ("C-f" . find-function)
              ("C-k" . find-function-on-key)
              ("C-v" . find-variable)))

(use-package misc
  :ensure nil
  :bind (("M-F" . forward-to-word)
         ("M-B" . backward-to-word)
         ("M-Z" . zap-up-to-char)))

(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

;; (declare-function wrap-region-add-wrapper "wrap-region")
;; (use-package wrap-region
;;   :diminish (wrap-region-mode)
;;   :functions (wrap-region-add-wrappers)
;;   :hook ((org-mode markdown-mode lisp-mode) . wrap-region-mode)
;;   :config
;;   (wrap-region-add-wrappers '(("+" "+" nil 'org-mode)
;;                               ("*" "*" nil 'org-mode)
;;                               ("/" "/" nil 'org-mode)
;;                               ("=" "=" nil 'org-mode)
;;                               ("_" "_" nil 'markdown-mode)
;;                               ("*" "*" nil 'markdown-mode)
;;                               ("`" "`" nil 'markdown-mode)
;;                               ("*" "*" nil 'lisp-mode)
;;                               ("+" "+" nil 'lisp-mode))))

(use-package wgrep
  :config
  (progn
    (defun mjs/add-wgrep-key ()
      (define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))
    (add-hook 'grep-mode-hook 'mjs/add-wgrep-key)))

(use-package browse-url
  :ensure nil
  :config
  (setq
   browse-url-new-window-flag t
   browse-url-secondary-browser-function 'browse-url-default-browser)
  (let ((browse-url-handler-alist '(("^mailto:" . browse-url-mail)
                                    ("github.com" . browse-url-default-browser)
                                    ("gitlab.com" . browse-url-default-browser)
                                    ("youtube.com" . browse-url-default-browser)
                                    ("exercism.org" . browse-url-default-browser)
                                    ("slack.com" . browse-url-default-browser)
                                    ("nyulmc.org" . browse-url-default-browser)
                                    ("google.com" . browse-url-default-browser)
                                    ("pivotaltracker.com" . browse-url-default-browser)
                                    ("." . eww-browse-url))))
    (setq browse-url-handlers browse-url-handler-alist)))

(use-package ediff
  :ensure nil
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package shell
  :ensure nil
  :config (add-to-list 'explicit-bash-args "--login"))

(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
              ("C-d" . isearch-forward-symbol-at-point))
  :config (setq isearch-lazy-count t))

(use-package ibuffer
  :ensure nil
  :bind (([remap list-buffers] . ibuffer)))

(setq scroll-preserve-screen-position t)

(use-package info
  :ensure nil
  :config (progn
            (add-to-list 'Info-directory-list (expand-file-name "~/.emacs.d/info"))
            (use-package info-look
              :ensure nil
              :commands (info-lookup-add-help)
              :config (info-lookup-add-help
                       :mode 'lisp-mode
                       :regexp "[^][()'\" \t\n]+"
                       :ignore-case t
                       :doc-spec '(("(ansicl)Symbol Index" nil nil nil))))))

(use-package server
  :ensure nil
  :init (add-hook 'after-init-hook 'server-start t))

(use-package dired
  :ensure nil
  :init (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)
  :config (setq dired-recursive-copies t
                dired-recursive-deletes t
                dired-deletion-confirmer #'y-or-n-p
                dired-dwim-target t
                dired-listing-switches "-alhv"
                dired-use-ls-dired nil
                dired-clean-up-buffers-too nil))

(use-package dired-x
  :ensure nil
  :after dired
  :demand t
  :config
  (push ".dvi" dired-latex-unclean-extensions)
  (push ".fls" dired-latex-unclean-extensions)
  (push ".fdb_latexmk" dired-latex-unclean-extensions))

(use-package dired-aux
  :ensure nil
  :after dired
  :demand t
  :config (setq dired-create-destination-dirs t))

(use-package calc-units
  :ensure nil
  :config (progn
            (setq math-additional-units
                  '((fort "14 day" "Fortnight")
                    (stone "14 lb" "Stone")
                    (bit nil "Bit")
                    (byte "8 bit" "Byte"))
                  math-units-table nil)))

(use-package browse-kill-ring
  :init (add-hook 'after-init-hook 'browse-kill-ring-default-keybindings ))

(use-package paren
  :ensure nil
  :init (add-hook 'after-init-hook 'show-paren-mode t))

(use-package autorevert
  :ensure nil
  :init (add-hook 'after-init-hook 'global-auto-revert-mode t))

(use-package hl-line
  :ensure nil
  :init (add-hook 'after-init-hook 'global-hl-line-mode t))

(use-package midnight
  :ensure nil
  :init (add-hook 'after-init-hook 'midnight-mode t))

(use-package miniedit
  :init (add-hook 'after-init-hook 'miniedit-install t))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package emacs
  :ensure nil
  :bind (("M-u" . #'upcase-dwim)
         ("M-l" . #'downcase-dwim))
  :config
  (setq
   describe-bindings-outline t
   echo-keystrokes 0.25
   save-interprogram-paste-before-kill t))

(use-package autoinsert
  :ensure nil
  :hook (find-file . auto-insert-mode))

(use-package display-fill-column-indicator
  :ensure nil
  :hook (after-init . global-display-fill-column-indicator-mode))

(use-package emacs
  :ensure nil
  :config
  (defun mjs/crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'mjs/crm-indicator)
  (setq read-extended-command-predicate #'command-completion-default-include-p))

(use-package minibuffer
  :ensure nil
  :bind (:map completion-in-region-mode-map
              ("M-n" . minibuffer-next-completion)
              ("M-p" . minibuffer-previous-completion)
              :map minibuffer-local-completion-map
              ("M-n" . minibuffer-next-completion)
              ("M-p" . minibuffer-previous-completion))
  :config
  (setq
   completion-ignore-case t
   read-file-name-completion-ignore-case t
   read-buffer-completion-ignore-case t

   completion-auto-select 'second-tab
   tab-always-indent 'complete
   tab-first-completion 'word

   completions-show-help nil
   completions-sort 'historical
   completions-detailed t
   completion-cycle-threshold 5)
  (push 'flex completion-styles))

(defun mjs/advice-after/dabbrev--find-expansion (&rest _args)
  (when (not (buffer-live-p dabbrev--last-buffer))
    (message "LAST BUFFER IS DEAD - removing it")
    (setq dabbrev--last-buffer nil)))

(use-package dabbrev
  :bind (("M-/" . #'dabbrev-completion)
         ("C-M-/" . #'dabbrev-expand))
  :config
  (advice-add #'dabbrev--find-expansion
              :after #'mjs/advice-after/dabbrev--find-expansion))

;; (use-package cape)

;; (use-package corfu
;;   :hook ((after-init . global-corfu-mode)
;;          (corfu-mode . corfu-popupinfo-mode))
;;   :config
;;   (setq corfu-auto t
;;         corfu-cycle t
;;         corfu-max-width 75))

;; (use-package vertico
;;   :hook (after-init . vertico-mode))

;; (use-package marginalia
;;   :hook (after-init . marginalia-mode))

(use-package completion-preview
  :ensure nil
  :diminish completion-preview-mode
  :hook (after-init . global-completion-preview-mode)
  :bind (:map completion-preview-active-mode-map
              ("M-n" . #'completion-preview-next-candidate)
              ("M-p" . #'completion-preview-prev-candidate))
  :config
  (push #'org-self-insert-command completion-preview-commands)
  (push #'paredit-backward-delete completion-preview-commands))

(defun mjs/advice-after/dabbrev--find-expansion (&rest _args)
  (when (not (buffer-live-p dabbrev--last-buffer))
    (message "LAST BUFFER IS DEAD - removing it")
    (setq dabbrev--last-buffer nil)))
(use-package dabbrev
  :autoload (dabbrev-capf)
  :config
  (advice-add #'dabbrev--find-expansion :after #'mjs/advice-after/dabbrev--find-expansion))

(use-package so-long
  :ensure nil
  :config (global-so-long-mode))

(use-package pulse
  :ensure nil
  :defer 5
  :init (setq pulse-command-advice-flag t)
  :config
  (dolist (command '(scroll-up-command scroll-down-command
                                       recenter-top-bottom other-window))
    (advice-add command :after #'(lambda (&rest _) (pulse-line-hook-function))))
  (advice-add 'yank :around #'(lambda (next-method &rest args)
                                (let ((begin (point))
                                      (retval (apply next-method args))
                                      (end (point)))
                                  (pulse-momentary-highlight-region begin end)
                                  retval))))
(use-package mode-line-bell
  :hook (after-init . mode-line-bell-mode))

(use-package which-key
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  :config (setq which-key-max-description-length 35))

(use-package diminish)

(use-package markdown-mode
  :hook ((markdown-mode . flymake-markdownlint-setup))
  :config (setq markdown-asymmetric-header t
                markdown-reference-location 'end))

(use-package flymake-markdownlint)

(use-package eshell
  :ensure nil
  :config (setq eshell-hist-ignoredups t))

(use-package :help
  :ensure nil
  :config (setq describe-bindings-outline t))

(require 'path-utils)

(require 'project-utils)
(use-package project
  :ensure nil
  :autoload (project-prompt-project-dir)
  :bind (:map project-prefix-map
              ("p" . #'mjs/project-switch-project)
              ("v" . #'magit-project-status)
              ("m" . #'magit-project-status)
              ("t" . #'mjs/project-run-tests))
  :config
  (setq project-switch-commands #'project-dired
        project-prompter #'project-prompt-project-name
        project-vc-extra-root-markers '(".project")))

(global-set-key (kbd "M-g l") #'mjs/project-file:line)

(use-package scratch
  :bind ("C-c s" . #'scratch))

(defun mjs/toggle-visual-fill-column-center-text ()
  (interactive)
  (setq visual-fill-column-center-text
        (not visual-fill-column-center-text))
      (visual-fill-column-adjust))

(use-package visual-fill-column
  :bind (:map visual-fill-column-mode-map
              ("s-c" . #'mjs/toggle-visual-fill-column-center-text))
  :hook (markdown-mode . visual-fill-column-mode))

(use-package dictionary
  :ensure nil
  :bind ("C-c d" . dictionary-search)
  :config (setq dictionary-server "dict.org"))

(defun mjs/webjump-to-rubocop-cop-doc ()
  (let* ((input (read-string "Cop " (thing-at-point 'symbol t)))
         (command (format "rubocop --show-doc-url %s" input)))
    (message (string-trim (shell-command-to-string command)))))

(use-package webjump
  :ensure nil
  :bind ("C-c j" . webjump)
  :config
  (add-to-list 'webjump-sites '("RuboCop Cop Doc" mjs/webjump-to-rubocop-cop-doc)))

(use-package graphviz-dot-mode)
(use-package gnuplot)

(use-package emacs-everywhere
  :config
  (setq emacs-everywhere-frame-parameters
        `((name . "emacs-everywhere")
          (fullscreen . nil) ; Helps on GNOME at least
          (width . 80)
          (height . 20))
        emacs-everywhere-major-mode-function
        #'emacs-everywhere-major-mode-org-or-markdown)
  (remove-hook 'emacs-everywhere-init-hooks 'emacs-everywhere-set-frame-position))

(defun mjs/fill-gptel-response (beg end)
  (fill-region beg end))

(use-package gptel
  :bind ("<f7>" . gptel-send)
  :hook (gptel-post-response-functions . mjs/fill-gptel-response)
  :config
  (setq gptel-model 'mistral:latest
        gptel-backend (gptel-make-ollama
                          "Ollama" :host "localhost:11434"
                          :stream t :models '(mistral:latest zephyr:latest))
        gptel-mode 'org-mode))
