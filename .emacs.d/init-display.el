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

(use-package unicode-fonts
  ;; with this also install Symbola & Quivira fonts
  :ensure t
  :config
  (unicode-fonts-setup))

(use-package modus-themes
  :defer nil
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-scale-headings t
        modus-themes-mode-line '(borderless)
        modus-themes-hl-line '(intense-backround)
        modus-themes-completions '((t . (moderate)))
        modus-themes-diffs 'desaturated)
  (modus-themes-load-themes)

  :config
  (modus-themes-load-vivendi))

(use-package auto-dark
  :demand t
  :init
  (setq auto-dark--light-theme 'modus-operandi
        auto-dark--dark-theme 'modus-vivendi))
