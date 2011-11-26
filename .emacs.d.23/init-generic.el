;;;
;;; Generic setup items needed/wanted all the time
;;; (both useful and bells-n-whistles)
;;;
;;;; Modified Time-stamp: <2011-05-05 22:08:05 mark>

(require 'highlight-tail)
(require 'ffap)
(require 'midnight)

;;; Key Definitions
;; (global-set-key (kbd "C-h a") 'apropos)
;; (global-set-key (kbd "C-c k") 'browse-kill-ring)
(define-key mode-specific-map "c" 'compile)
;; (define-key mode-specific-map "g" 'goto-line)
(define-key mode-specific-map ";" 'comment-dwim)
(global-set-key (kbd "<f7>") 'calendar)
;; (define-key isearch-mode-map (kbd "C-o")           
;;   (lambda ()                                         
;;     (interactive)                                      
;;     (let ((case-fold-search isearch-case-fold-search)) 
;;       (occur (if isearch-regexp isearch-string           
;; 		 (regexp-quote isearch-string))))))
(defun switch-to-gnus () 
  (interactive) 
  (let ((group-buffer (get-buffer "*Group*")))
    (if group-buffer (switch-to-buffer group-buffer)
	(gnus))))
(global-set-key (kbd "<f6>") 'switch-to-gnus)

(defun toggle-transparency ()
  (interactive)
  (if (/=
       (cadr (frame-parameter nil 'alpha))
       100)
      (set-frame-parameter nil 'alpha '(100 100))
    (set-frame-parameter nil 'alpha '(85 50))))
(global-set-key (kbd "C-c t") 'toggle-transparency)

;;; Auto Mode Alist
(add-to-list 'auto-mode-alist '("\\.plist" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.org" . org-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))


;;; Display settings
(setq default-frame-alist 
      '(
	;; (foreground-color . "white") 
	;; (background-color . "black") 
	(alpha 85 50)
	(vertical-scroll-bars) 
	(tool-bar-lines . 0) 
	(menu-bar-lines . 0) ))

(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message "damned"
      battery-mode-line-format " [%b%p%%,%d°C [%t]]"
      default-major-mode 'text-mode
      delete-by-moving-to-trash t
      trash-directory (expand-file-name "~/.Trash")
      delete-old-versions t
      display-time-24hr-format t
      display-time-day-and-date t
      canlock-password "ce279072fea69d713f49a23a12637f389ac53e53"
      minibuffer-complete-cycle t
      uniquify-after-kill-buffer-p t
      uniquify-buffer-name-style 'post-forward-angle-brackets
      version-control t
      x-select-enable-clipboard t
      ediff-window-setup-funtion #'ediff-setup-windows-plain
      ns-pop-up-frames nil
      ps-lpr-command (expand-file-name "~/bin/psprint")
      )

(add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)
(add-hook 'emacs-lisp-mode-hook #'checkdoc-minor-mode)

(add-hook 'midnight-hook #'bbdb-expire-bbdb)
(add-hook 'midnight-hook #'clean-buffer-list)

(add-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook #'text-mode-hook-identify)
(add-hook 'text-mode-hook #'footnote-mode)

(add-hook 'write-file-hooks 'time-stamp)


(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)

(provide 'init-generic)
