;;;;
;;;; Latex
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(use-package tex
  :ensure auctex
  :commands TeX-global-PDF-mode
  :config
  (progn (TeX-global-PDF-mode 1)
         (setq TeX-auto-save t
               TeX-parse-self t
               ;; the following is to fix a strange bug/scenario where error
               ;; messages are shown in Japanese!
               japanese-TeX-error-messages nil)

         ;;;
         ;;; experimental
         ;;;
         (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
               TeX-source-correlate-start-server t)

         ;; Update PDF buffers after successful LaTeX runs
         (add-hook 'TeX-after-compilation-finished-functions
                   #'TeX-revert-document-buffer)
))

(defun mjs/set-latex-comment-start ()
  (setq-local comment-start "% "))

(use-package latex
  :ensure auctex
  :init
  (setq
   ;; used in my AD&D documents
   font-latex-user-keyword-classes
   '(("hexchapter" (("hexchapter" "[{")) font-latex-sectioning-1-face command)
     ("hexion" (("hexion" "[{")) font-latex-sectioning-2-face command)
     ("level" (("level" "[{")) font-latex-sectioning-1-face)))
  :config
  (progn
    (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
    (add-hook 'LaTeX-mode-hook 'flymake-mode)
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (add-hook 'LaTeX-mode-hook 'auctex-latexmk-setup)
    (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
    (add-hook 'LaTeX-mode-hook 'display-line-numbers-mode)
    (add-hook 'LaTeX-mode-hook 'mjs/set-latex-comment-start)
    (add-to-list 'LaTeX-section-list '("level" 1))
    (add-to-list 'LaTeX-section-list '("hexchapter" 1))
    (add-to-list 'LaTeX-section-list '("hexion" 2))))

(use-package reftex
  :config (setq reftex-plug-into-AUCTeX t))

(use-package auctex)
(use-package auctex-latexmk)
(use-package pdf-tools)

(define-skeleton latex-todo-comment "Inserts a TODO comment" ""
  > (if (> comment-add 0) (s-trim comment-start)) comment-start
  "TODO: " _ comment-end)
