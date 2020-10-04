;; make sure the display is clean to start with
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message "damned")

(set-face-attribute 'default nil
                    :height 180
                    :family "DejaVu Sans Mono")
(set-face-attribute 'fixed-pitch nil
                    :height 180
                    :family "DejaVu Sans Mono")
(set-face-attribute 'variable-pitch nil
                    :height 180
                    :family "DejaVu Sans")

(defun set-modus-theme-settings (theme &rest settings)
  (when settings
    (cl-destructuring-bind (var value &rest other) settings
      (set (intern (format "modus-%s-theme-%s" theme var)) value)
      (apply #'set-modus-theme-settings theme other))))

(use-package modus-operandi-theme
  :defer nil
  :init
  (set-modus-theme-settings 'operandi
                            'slanted-constructs t
                            'bold-constructs t
                            'proportional-fonts t
                            'rainbow-headings t
                            'scale-headings t
                            'visible-fringes t
                            'distinct-org-blocks t
                            '3d-modeline t
                            'intense-hl-line t
                            'intense-standard-completions t)
  :config (load-theme 'modus-operandi t t))

(use-package modus-vivendi-theme
  :defer nil
  :init
  (set-modus-theme-settings 'vivendi
                            'slanted-constructs t
                            'bold-constructs t
                            'proportional-fonts t
                            'rainbow-headings t
                            'scale-headings t
                            'visible-fringes t
                            'distinct-org-blocks t
                            '3d-modeline t
                            'intense-hl-line t
                            'intense-standard-completions t)
  :config (load-theme 'modus-vivendi t t))

(enable-theme 'modus-vivendi)
(enable-theme 'modus-operandi)

(defun mjs/toggle-theme ()
  (interactive)
  (enable-theme (cl-second custom-enabled-themes)))
