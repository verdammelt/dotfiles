;;;;
;;;; IDO
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(defun mjs/ido-init ()
  (ido-vertical-mode)
  (ido-sort-mtime-mode)

  (ido-mode)
  (ido-everywhere)
  (ido-ubiquitous-mode)
  (ido-hacks-mode)
  (flx-ido-mode 1)
  (smex-initialize))

(use-package ido
  :commands (ido-everywhere)
  :init (add-hook 'after-init-hook 'mjs/ido-init t)
  :config
  (progn
    (setq ido-show-dot-for-dired t
          ido-enable-flex-matching t)))

(use-package ido-vertical-mode
  :config
  (setq ido-vertical-define-keys t))

(use-package ido-hacks
  :commands (ido-hacks-mode)
  :bind (("M-x" . smex)))

(use-package ido-sort-mtime)
(use-package ido-completing-read+)
(use-package flx-ido)
(use-package smex
  :config (setq smex-save-file (locate-user-emacs-file ".smex-items")))
