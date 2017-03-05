;; make sure the display is clean to start with
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-splash-screen t
      inhibit-startup-echo-area-message "damned")

;; Fallback font - helps with Unicode
(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "Symbola"))

(set-face-attribute 'default nil
                    :height 180
                    :family "DejaVu Sans Mono")

(load-theme 'tango-2)
(set-cursor-color "red")
