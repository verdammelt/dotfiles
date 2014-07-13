;;;;
;;;; MISC
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Modified Time-stamp: <2014-07-12 16:48:44 mark>
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

;; making sure all buffers are named uniquely
(after 'uniquify 
  (setq uniquify-after-kill-buffer-p t
	uniquify-buffer-name-style 'post-forward-angle-brackets))

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
  (defun ac-text-mode-setup ()
    (add-to-list 'ac-sources 'ac-source-words-in-buffer)
    (ac-ispell-ac-setup)
    (add-to-list 'ac-sources 'ac-source-yasnippet))
  (add-hook 'text-mode-hook 'ac-text-mode-setup))

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
  (add-hook 'coffee-mode-hook 'whitespace-mode)
  (setq whitespace-style '(face empty indentation trailing)
	whitespace-action '(auto-cleanup warn-if-read-only))
  (setq coffee-tab-width 4))

;; markdown-mode
(after 'markdown-mode
  (setq markdown-command "markdown | smartypants"))

(defun setup-text-mode ()
  (setq sentence-end-double-space nil))
(add-hook 'text-mode-hook 'setup-text-mode)
(add-hook 'text-mode-hook 'turn-on-fci-mode)

(after 'ns-win 
  (setq mac-command-modifier 'meta
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

(defun change-size (size)
  (interactive "nsize: ")
  (if (< size 100) (setq size (* 10 size)))
  (set-face-attribute 'default nil :height size))
(global-set-key (kbd "H-s") 'change-size)

(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-i") 'change-inner)
(global-set-key (kbd "M-o") 'change-outer)
(global-set-key (kbd "s-g") 'goto-line)
(global-set-key (kbd "s-j") 'ace-jump-word-mode)
(global-set-key (kbd "s-J") 'ace-jump-char-mode)

(define-key 'help-command (kbd "C-l") 'find-library)
(define-key 'help-command (kbd "C-f") 'find-function)
(define-key 'help-command (kbd "C-k") 'find-function-on-key)
(define-key 'help-command (kbd "C-v") 'find-variable)

(provide 'init-misc)
