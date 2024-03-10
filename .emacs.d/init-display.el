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

(set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)

(setq custom-safe-themes '("69f7e8101867cfac410e88140f8c51b4433b93680901bb0b52014144366a08c8"
                           default))

(defun mjs/set-mode-line-highlight ()
  (set-face-attribute
   'mode-line nil
   :foreground "black" :background "goldenrod"
   :box '(:line-width 2 :color "black")))

(use-package modus-themes
  :hook ((modus-themes-after-load-theme . mjs/set-mode-line-highlight))
  :defer nil
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
        modus-themes-scale-headings t
        modus-themes-mode-line '(borderless)
        modus-themes-hl-line '(intense-backround)
        modus-themes-completions '((t . (moderate)))
        modus-themes-diffs 'desaturated)

  :config
  (modus-themes-select 'modus-vivendi))

(use-package auto-dark
  :demand t
  :hook ((auto-dark-dark-mode . mjs/set-mode-line-highlight)
         (auto-dark-light-mode . mjs/set-mode-line-highlight))
  :diminish (auto-dark-mode)
  :init
  (setq auto-dark-light-theme 'modus-operandi
        auto-dark-dark-theme 'modus-vivendi)
  (auto-dark-mode))
