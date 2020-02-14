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

(use-package color-theme-sanityinc-tomorrow
  :defer nil
  :config (load-theme 'sanityinc-tomorrow-bright))

(cl-defun mjs/bg-color-box (face &optional (width 5))
  (list :line-width width :color (face-attribute face :background)))

(set-face-attribute 'mode-line nil
                    :box (mjs/bg-color-box 'mode-line))

(set-face-attribute 'mode-line-inactive nil
                    :box (mjs/bg-color-box 'mode-line-inactive))
