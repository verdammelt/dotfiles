;;;;
;;;; IDO
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(use-package ido
  :commands (ido-everywhere ido-next-match ido-prev-match ido-toggle-prefix)
  :hook ((after-init . ido-mode)
         (after-init . ido-everywhere))
  :bind (:map ido-common-completion-map
              (("\C-n" . 'ido-next-match)
               ("\C-p" . 'ido-prev-match)
               ("C-S-p" . 'ido-toggle-prefix)))
  :config
  (progn
    (setq ido-show-dot-for-dired t
          ido-enable-flex-matching t
          ido-use-filename-at-point 'guess
          ido-use-url-at-point t)))

(use-package ido-completing-read+
  :hook ((after-init . ido-ubiquitous-mode)))
