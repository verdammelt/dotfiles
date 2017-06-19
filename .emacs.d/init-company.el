(declare-function turn-on-fci-mode "fill-column-indicator")
(declare-function turn-off-fci-mode "fill-column-indicator")

(defun on-off-fci-before-company(command)
  "Turn fci-mode off/on when company mode is showing/hiding its popup.
Need this due to a bug/incompatibility between company-mode and fci-mode."
  (when (string= "show" command) (turn-off-fci-mode))
  (when (string= "hide" command) (turn-on-fci-mode)))

(use-package company
  :defer 2
  :diminish (company-mode)
  :bind (:map company-active-map
              ("\C-n" . company-select-next)
              ("\C-p" . company-select-previous)
              ("\C-d" . company-show-doc-buffer)
              ("M-." . company-show-location))

  :config
  (progn
    (advice-add 'company-call-frontends :before #'on-off-fci-before-company)

    (set-face-attribute 'company-tooltip nil :background "white" :foreground "black")
    (set-face-attribute 'company-tooltip-selection nil :background "grey" :foreground "red")
    (set-face-attribute 'company-tooltip-common nil :slant 'italic :foreground "blue")
    (set-face-attribute 'company-scrollbar-fg nil :background "black")
    (set-face-attribute 'company-scrollbar-bg nil :background "grey")
    (set-face-attribute 'tooltip nil :background "white" :inherit 'default)

    (setq company-idle-delay .25
          company-show-numbers t
          company-tooltip-align-annotations t
          company-selection-wrap-around t)

    (global-company-mode)
    (company-quickhelp-mode)))
