;;;;
;;;; Latex
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(use-package tex
  :ensure auctex
  :commands TeX-global-PDF-mode
  :init (setq-default TeX-master nil)
  :config
  (progn (TeX-global-PDF-mode 1)
         (setq TeX-auto-save t
               TeX-parse-self t)))

(use-package latex
  :ensure auctex
  :config
  (progn
    (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)))

(use-package reftex
  :ensure nil
  :config (setq reftex-plug-into-AUCTeX t))

(use-package auctex)
