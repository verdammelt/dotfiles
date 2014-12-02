;;;;
;;;; MISC
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(setq c-default-style "linux"
      c-basic-offset 4)

;; Save my place in files
(require 'saveplace)
(setq-default save-place t)
(after 'saveplace
  (setq save-place-file (locate-user-emacs-file ".places")))

;; Save minibuffer history
(setq savehist-file (locate-user-emacs-file ".history"))

;; Backup files
(after 'files
  (setq version-control t
	delete-old-versions t
	backup-directory-alist
	(acons "." (locate-user-emacs-file ".backups") nil)
	delete-by-moving-to-trash t
	trash-directory (expand-file-name "~/.Trash")))

;; time display the way i like it
(after 'time
  (setq display-time-24hr-format t
	display-time-day-and-date t))

(global-set-key (kbd "s-p") 'ps-print-buffer)
(global-set-key (kbd "s-P") 'ps-print-region)
(after 'ps-print
  (setq
   ps-lpr-command (expand-file-name "~/bin/psprint")
   ps-spool-duplex t))

;; calendar
(after 'calendar
  (setq
   calendar-latitude +40.72541
   calendar-longitude -73.70928
   calendar-location-name "Floral Park, NY"
   calendar-time-display-form '(24-hours ":" minutes (if time-zone " (") time-zone (if time-zone ")"))
   diary-file "~/.diary"
   calendar-date-style 'european
   calendar-mark-diary-entries-flag t
   calendar-mark-holidays-flag t
   calendar-view-diary-initially-flag t
   calendar-view-holidays-initially-flag t))

;; Editing text
(setq fill-column 78)
(add-hook 'text-mode-hook 'turn-on-fci-mode)
(after 'fill-column-indicator (setq fci-rule-color "red"))

(after 'battery
  (setq battery-mode-line-format "[%b%p%% %t]"))

;; editing programs
(after 'simple
  (add-hook 'prog-mode-hook 'linum-mode)
  (add-hook 'prog-mode-hook 'turn-on-fci-mode))

;; auto-complete
(require 'auto-complete-config)
(ac-config-default)
(after 'auto-complete
  (ac-ispell-setup)
  (setf (cdr (assoc 'symbol ac-source-ispell)) "d")
  (setq ac-use-menu-map t
	ac-auto-show-menu t)
  (defun mjs/ac-text-mode-setup ()
    (add-to-list 'ac-sources 'ac-source-words-in-buffer)
    (ac-ispell-ac-setup)
    (add-to-list 'ac-sources 'ac-source-yasnippet))
  (add-hook 'text-mode-hook 'mjs/ac-text-mode-setup))

;; abbrevs
(setq-default abbrev-mode t)

;; flyspell
(add-hook 'text-mode-hook 'flyspell-mode)
(after 'flyspell
  (setq flyspell-abbrev-p t))

;; yasnippet
(after 'yasnippet
  (setq yas-prompt-functions
	'(yas-ido-prompt yas-completing-prompt)))

(global-set-key (kbd "<f7>") 'magit-status)
(after 'magit
  (setq magit-default-tracking-name-function
	'magit-default-tracking-name-branch-only))

(after 'coffee-mode
  (add-to-list 'ac-modes 'coffee-mode)
  (setq coffee-tab-width 4))

(after 'simple
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (setq-default indent-tabs-mode nil)
  (setq whitespace-style '(face empty trailing)
	whitespace-action '(auto-cleanup warn-if-read-only)))


;; markdown-mode
(after 'markdown-mode
  (setq markdown-command "markdown | smartypants"))

(defun mjs/setup-text-mode ()
  (setq sentence-end-double-space nil))
(add-hook 'text-mode-hook 'mjs/setup-text-mode)
(add-hook 'text-mode-hook 'turn-on-fci-mode)

(after 'ns-win
  (setq ns-use-srgb-colorspace t
	mac-command-modifier 'meta
	mac-option-modifier 'super
	mac-function-modifier 'hyper))

(after 'smex
  (setq smex-save-file (locate-user-emacs-file ".smex-items")))

(setq projectile-keymap-prefix (kbd "C-c C-p")
      projectile-known-projects-file
	(locate-user-emacs-file ".projectile-bookmarks.eld")
	projectile-cache-file
	(locate-user-emacs-file ".projectile.cache"))
(global-set-key (kbd "s-b") 'projectile-switch-to-buffer)
(global-set-key (kbd "s-f") 'projectile-find-file)
(global-set-key (kbd "s-s") 'projectile-switch-project)
(after 'projectile
  (setq mjs/default-projectile-indexing-method projectile-indexing-method)
  (defun mjs/setup-gtd-project-caching ()
    (let ((new-value (if (string= (projectile-project-name) "GTD")
			 'native
		       mjs/default-projectile-indexing-method)))
      (setq projectile-indexing-method new-value)))
  (add-hook 'projectile-switch-project-hook 'mjs/setup-gtd-project-caching)

  (setq projectile-switch-project-action 'projectile-dired
	projectile-enable-caching t))

;; Update timestamps in file on save
(add-hook 'before-save-hook 'time-stamp)

;; Misc
(after 'gnutls
  (setq gnutls-min-prime-bits 1024))
(put 'narrow-to-region 'disabled nil)

(defalias 'yes-or-no-p 'y-or-n-p)

(defun mjs/change-size (size)
  (interactive "nsize: ")
  (if (< size 100) (setq size (* 10 size)))
  (set-face-attribute 'default nil :height size))
(global-set-key (kbd "H-s") 'mjs/change-size)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)
(global-set-key (kbd "s-j") 'ace-jump-word-mode)
(global-set-key (kbd "s-J") 'ace-jump-char-mode)

(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurance of CHAR.")
(autoload 'forward-to-word "misc"
  "Move forward until encountering the beginning of a word.
With argument, do this that many times.")
(autoload 'backward-to-word "misc"
  "Move backward until encountering the end of a word.
With argument, do this that many times.")
(global-set-key (kbd "M-F") 'forward-to-word)
(global-set-key (kbd "M-B") 'backward-to-word)
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;; multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

;; wrap-region
(after 'wrap-region
  (wrap-region-add-wrapper "+" "+" nil 'org-mode)
  (wrap-region-add-wrapper "_" "_" nil 'markdown-mode)
  (wrap-region-add-wrapper "*" "*" nil 'markdown-mode))

(defun mjs/add-wgrep-key ()
  (define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))
(add-hook 'grep-mode-hook 'mjs/add-wgrep-key)

;; always want browser to open in background.
(defun mjs/browse-url-default-macosx-browser-background (url &optional _new-window)
  (interactive (browse-url-interactive-arg "URL: "))
  (start-process (concat "open " url) nil "open" "-g" url))
(setq browse-url-browser-function 'mjs/browse-url-default-macosx-browser-background)

(after 'ediff
  (setq ediff-window-setup-function 'ediff-setup-windows-plain))

(after 'shell
  (add-to-list 'explicit-bash-args "--login"))

;; search for symbol at point (by Jorgen Schäfer)
(define-key isearch-mode-map (kbd "C-d") 'fc/isearch-yank-symbol)
(defun fc/isearch-yank-symbol ()
  "Yank the symbol at point into the isearch minibuffer.

C-w does something similar in isearch, but it only looks for
the rest of the word. I want to look for the whole string. And
symbol, not word, as I need this for programming the most."
  (interactive)
  (isearch-yank-string
   (save-excursion
     (when (and (not isearch-forward) isearch-other-end)
       (goto-char isearch-other-end))
     (thing-at-point 'symbol))))
