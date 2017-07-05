;;;;
;;;; IDO
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(use-package ido
  :defer 2
  :commands (ido-everywhere)
  :config
  (progn
    (setq ido-show-dot-for-dired t
          ido-enable-flex-matching t)
    (ido-vertical-mode)
    (ido-sort-mtime-mode)

    (ido-mode)
    (ido-everywhere)
    (ido-ubiquitous-mode)
    (ido-hacks-mode)
    (flx-ido-mode 1)
    (smex-initialize)))

(use-package ido-vertical-mode
  :config
  (setq ido-vertical-define-keys t))

(use-package ido-hacks
  :commands (ido-hacks-mode)
  :bind (("M-x" . smex)))

(use-package ido-sort-mtime)
(use-package ido-ubiquitous)
(use-package flx-ido)
(use-package smex
  :config (setq smex-save-file (locate-user-emacs-file ".smex-items")))
