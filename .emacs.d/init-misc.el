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
  :bind (("s-p" . ps-print-buffer)
         ("s-P" . ps-print-region))
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
               trash-directory (expand-file-name "~/.Trash"))))

(use-package paragraphs
  :ensure nil
  :init (setq sentence-end-double-space nil))

(use-package battery
  :ensure nil
  :config
  (progn
    (setq battery-mode-line-format "[%b%p%% %t] ")
    (display-battery-mode)))

(use-package abbrev
  :ensure nil
  :init (setq-default abbrev-mode t)
  :diminish (abbrev-mode))

;; flyspell
(use-package flyspell
  :ensure nil
  :diminish (flyspell-mode)
  :config
  (progn
    (add-hook 'text-mode-hook 'flyspell-mode)
    (setq flyspell-use-meta-tab nil
          flyspell-abbrev-p t)))

(use-package yasnippet
  :diminish (yas-minor-mode)
  :init (add-hook 'after-init-hook 'yas-global-mode t))

(use-package magit
  :bind (("C-x g" . magit-status))
  :functions (magit-mode-quit-window)
  :config
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1) )

(use-package forge
  :after magit)

(use-package git-commit
  :init (add-hook 'after-init-hook 'global-git-commit-mode t))

(use-package simple
  :ensure nil
  :hook (markdown-mode . visual-line-mode)
  :init
  (setq-default indent-tabs-mode nil)
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow)))

(use-package whitespace
  :diminish (whitespace-mode)
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

(defalias 'yes-or-no-p 'y-or-n-p)

(defun mjs/change-size (size)
  (interactive "nsize: ")
  (if (< size 100) (setq size (* 10 size)))
  (dolist (face '(default fixed-pitch variable-pitch))
    (set-face-attribute face nil :height size)))
(global-set-key (kbd "H-s") 'mjs/change-size)

(use-package expand-region
  :bind (("C-=" . er/expand-region)))

(use-package change-inner
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

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
  :bind (("C-S-c C-S-c" . mc/mark-all-like-this-dwim)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package wrap-region
  :diminish (wrap-region-mode)
  :functions (wrap-region-add-wrappers)
  :init (add-hook 'after-init-hook 'wrap-region-global-mode t)
  :config (wrap-region-add-wrappers '(("+" "+" nil 'org-mode)
                                      ("_" "_" nil 'markdown-mode)
                                      ("*" "*" nil 'markdown-mode))))

(use-package grep
  :ensure nil
  :config
  (progn
    (defun mjs/add-wgrep-key ()
      (define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))
    (add-hook 'grep-mode-hook 'mjs/add-wgrep-key)))

(use-package browse-url
  :ensure nil
  :config
  (setq browse-url-browser-function '(("file://.*" . eww-browse-url)
                                      ("." . browse-url-default-browser))
        browse-url-secondary-browser-function 'eww-browse-url))

(use-package ediff
  :ensure nil
  :config (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(use-package shell
  :ensure nil
  :config (add-to-list 'explicit-bash-args "--login"))

(use-package isearch
  :ensure nil
  :bind (:map isearch-mode-map
              ("C-d" . isearch-forward-symbol-at-point)))

(use-package ibuffer
  :ensure nil
  :bind (([remap list-buffers] . ibuffer)))

(setq scroll-preserve-screen-position t)

(use-package info
  :ensure nil
  :config (progn
            (add-to-list 'Info-directory-list (expand-file-name "~/.emacs.d/info"))
            (use-package info-look
              :commands (info-lookup-add-help)
              :config (info-lookup-add-help
                       :mode 'lisp-mode
                       :regexp "[^][()'\" \t\n]+"
                       :ignore-case t
                       :doc-spec '(("(ansicl)Symbol Index" nil nil nil))))))

(use-package edit-server
  :ensure nil
  :init (add-hook 'after-init-hook 'edit-server-start t)
  :config (progn
          (add-hook 'edit-server-start-hook 'edit-server-maybe-dehtmlize-buffer)
          (add-hook 'edit-server-done-hook 'edit-server-maybe-htmlize-buffer)
          (add-hook 'edit-server-done-hook (lambda () (kill-ring-save (point-min) (point-max))))))

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
                dired-listing-switches "-alv"
                dired-use-ls-dired nil))
(use-package dired-x
  :ensure nil
  :after dired
  :config (setq dired-clean-up-buffers-too nil))

(use-package calc-units
  :ensure nil
  :config (progn
            (setq math-additional-units
                  '((fort "14 day" "Fortnight")
                    (stone "14 lb" "Stone")
                    (bit nil "Bit")
                    (byte "8 bit" "Byte"))
                  math-units-table nil)))

(use-package flycheck
  :init (add-hook 'after-init-hook 'global-flycheck-mode t)
  :config (progn
            (setq flycheck-completing-read-function 'ido-completing-read)
            (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
            (flycheck-credo-setup)))

(use-package browse-kill-ring
  :init (add-hook 'after-init-hook 'browse-kill-ring-default-keybindings ))

(use-package paren-mode
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
  :hook (after-init . delete-selection-mode))

(use-package emacs
  :config
  (setq echo-keystrokes 0.25
        save-interprogram-paste-before-kill t))

(use-package autoinsert
  :ensure nil
  :hook (find-file . auto-insert-mode))

(use-package display-fill-column-indicator
  :hook (after-init . global-display-fill-column-indicator-mode))

(use-package icomplete
  :ensure nil
  :hook (after-init . fido-mode)
  :functions (icomplete-forward-completions icomplete-backward-completions)
  :bind (:map icomplete-fido-mode-map
              ("C-n" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions))
  :config (setq icomplete-in-buffer t))
