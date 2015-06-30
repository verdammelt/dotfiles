;;;;
;;;; IDO
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(with-eval-after-load 'ido
  (setq ido-show-dot-for-dired t
        ido-enable-flex-matching t)
  (ido-vertical-mode)
  (ido-sort-mtime-mode))

(autoload 'ido-hacks-mode "ido-hacks")
(defun mjs/setup-smex ()
  (global-set-key (kbd "M-x") 'smex))
(add-hook 'ido-hacks-mode-hook 'mjs/setup-smex)

(ido-mode)
(ido-everywhere)
(ido-ubiquitous-mode)
(ido-hacks-mode)
(flx-ido-mode 1)
(smex-initialize)			; must be after ido-hacks!
