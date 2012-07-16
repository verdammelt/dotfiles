;;;
;;; Latex
;;;
;;; Modified Time-stamp: <2011-12-09 22:13:59 mark>
;;;
(eval-after-load 'tex '(TeX-global-PDF-mode))

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(provide 'init-latex)
