(with-eval-after-load 'company
  ;; due to a bug/incompatibility between company-mode and fci-mode
  (defun on-off-fci-before-company(command)
    (when (string= "show" command)
      (turn-off-fci-mode))
    (when (string= "hide" command)
      (turn-on-fci-mode)))
  (advice-add 'company-call-frontends :before #'on-off-fci-before-company)

  (set-face-attribute 'company-tooltip nil :background "white" :foreground "black")
  (set-face-attribute 'company-tooltip-selection nil :background "grey" :foreground "red")
  (set-face-attribute 'company-tooltip-common nil :slant 'italic :foreground "blue")
  (set-face-attribute 'company-scrollbar-fg nil :background "black")
  (set-face-attribute 'company-scrollbar-bg nil :background "grey")
  (set-face-attribute 'tooltip nil :background "white" :inherit 'default)
  (defvar company-active-map)
  (defvar company-idle-delay)
  (defvar company-show-numbers)
  (defvar company-selection-wrap-around)
  (define-key company-active-map (kbd "\C-n") 'company-select-next)
  (define-key company-active-map (kbd "\C-p") 'company-select-previous)
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-.") 'company-show-location)
  (diminish 'company-mode)
  (setq company-idle-delay .25
        company-show-numbers t
        company-tooltip-align-annotations t
        company-selection-wrap-around t))
