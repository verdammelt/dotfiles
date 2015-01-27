;;;;
;;;; IDO
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(after 'ido
  (setq ido-show-dot-for-dired t
	ido-enable-flex-matching t)
  (ido-vertical-mode)
  (ido-sort-mtime-mode))

(autoload 'ido-hacks-mode "ido-hacks")
(defun mjs/setup-smex ()
  (global-set-key (kbd "M-x") 'smex))
(add-hook 'ido-hacks-mode-hook 'mjs/setup-smex)
