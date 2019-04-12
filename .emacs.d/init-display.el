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

(load-theme 'tango-dark)
(set-background-color "black")
(set-face-attribute 'highlight nil :background "thistle")

(cl-defun mjs/bg-color-box (face &optional (width 5))
  (list :line-width width :color (face-attribute face :background)))

(set-face-attribute 'mode-line nil
                    :box (mjs/bg-color-box 'mode-line))

(set-face-attribute 'mode-line-inactive nil
                    :box (mjs/bg-color-box 'mode-line-inactive))

;; (setq-default mode-line-format
;;               (list
;;                (propertize (format-time-string "<%Y-%m-%dT%H:%M>")
;;                            'face 'font-lock-builtin-face)
;;                " "
;;                "%b"
;;                " "
;;                "("
;;                (propertize "%02l" 'face 'font-lock-keyword-face)
;;                ","
;;                (propertize "%02c" 'face 'font-lock-keyword-face)
;;                ")"
;;                " "
;;                "["
;;                (propertize "%p" 'face 'font-lock-keyword-face) ;; % above top
;;                "/"
;;                (propertize "%I" 'face 'font-lock-keyword-face) ;; size
;;                "]"
;;                '(:eval (propertize
;;                         " "
;;                         'display
;;                         `((space :align-to (- (+ right right-fringe right-margin)
;;                                               ,(+ 2 (string-width mode-name)))))))
;;                (propertize "%m" 'face 'font-lock-constant-face)
;;                ))

;; (setq-default mode-line-format
;;               '("%e"
;;                 mode-line-front-space
;;                 mode-line-mule-info
;;                 mode-line-client
;;                 mode-line-modified
;;                 mode-line-remote
;;                 mode-line-frame-identification
;;                 ;; mode-line-buffer-identification
;;                 ;; "   "
;;                 ;; mode-line-position
;;                 vc-mode
;;                 "  "
;;                 mode-line-misc-info
;;                 mode-line-end-spaces

;;                 ;; (propertize "foo"
;;                 ;;             'face 'font-lock-doc-face)

;;                 ;; '(:eval (propertize
;;                 ;;          " "
;;                 ;;          'display
;;                 ;;          `((space :align-to (- (+ right right-fringe right-margin)
;;                 ;;                                ,(+ 3 (string-width mode-name)))))))
;;                 ;; '(:eval (propertize
;;                 ;;           " "
;;                 ;;           'display
;;                 ;;           `((space :align-to (- (+ right right-fringe right-margin)
;;                 ;;                                 (+ 3 4))))))
;;                 ;; (propertize mode-name 'face 'font-lock-constant-face)
;;                 ))

(set-cursor-color "red")
