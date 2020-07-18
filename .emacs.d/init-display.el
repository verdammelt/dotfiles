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


(use-package modus-operandi-theme :defer nil)
(use-package modus-vivendi-theme :defer nil)
;; (load-theme 'modus-vivendi)
(load-theme 'modus-operandi)
