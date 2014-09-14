;;;;
;;;; Latex
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(after 'tex 
  (TeX-global-PDF-mode)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

(after 'latex 
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (after 'reftex
    (setq reftex-plug-into-AUCTeX t)))
