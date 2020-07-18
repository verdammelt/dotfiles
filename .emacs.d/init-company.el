(use-package company
  :diminish (company-mode)
  :bind (("M-/" . company-complete)
         :map company-active-map
              ("C-n" . company-select-next)
              ("C-." . company-select-next)
              ("C-p" . company-select-previous)
              ("C-," . company-select-previous)
              ("C-d" . company-show-doc-buffer)
              ("M-." . company-show-location))
    :init (add-hook 'after-init-hook 'global-company-mode t)
  :config
  (progn
    (setq company-idle-delay .1
          company-tooltip-idle-delay .25
          company-tooltip-limit 20
          company-show-numbers t
          company-tooltip-align-annotations t
          company-selection-wrap-around t)))
