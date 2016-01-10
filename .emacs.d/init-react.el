;;;
;;; We will need the NVM environment to run eslint. (Here we cavalierly
;;; choose the last nvm version from the installed list.)
;;;
(require 'nvm)
(nvm-use (caar (last (nvm--installed-versions))))

;;;
;;; I don't like installing project dev tools globally (I have run into
;;; a problem or two with global vs. local grunt for example.) So I
;;; check for an eslint installed into the projects node_modules
;;; directory and use that as the executable for flycheck.
;;;
(with-eval-after-load 'projectile
  (add-hook 'projectile-after-switch-project-hook 'mjs/setup-local-eslint))

(defvar flycheck-javascript-eslint-executable)
(defun mjs/setup-local-eslint ()
    "If ESLint found in node_modules directory - use that for flycheck.
Intended for use in PROJECTILE-AFTER-SWITCH-PROJECT-HOOK."
    (interactive)
    (let ((local-eslint (expand-file-name "./node_modules/.bin/eslint")))
      (setq flycheck-javascript-eslint-executable
            (and (file-exists-p local-eslint) local-eslint))))

;;;
;;; web-mode can handle the mix of JS code and HTML markup which one
;;; finds in React/ReactNative code files.
;;;
;;; Since I don't see a clear winner between naming React/ReactNative
;;; files *.js or *.jsx I will use web-mode for both.
;;;
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
(with-eval-after-load 'web-mode
  ;; set reasoable indentation for web-mode
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2)

  ;; Out of the box flycheck's eslint is restricted to specific modes
  ;; which does not include web-mode.
  (with-eval-after-load 'flycheck
    (push 'web-mode (flycheck-checker-get 'javascript-eslint 'modes))))

;;;
;;; Javascript mode is used for JSON files. I'd like its identation to
;;; match the code.
;;;
(with-eval-after-load 'js
  (setq js-indent-level 2))

;;;
;;; As I am not entirely sure web-mode is the solution I have coded up
;;; a utility function to toggle between the two modes.
;;;
;;; (This code can be removed when I'm sure I'm happy with one mode or
;;; the other.)
(defun mjs/toggle-web-mode ()
  "Toggle between js-mode and web-mode. Do nothing if in neither mode already."
  (interactive)
  (cond ((eq major-mode 'web-mode) (js-mode))
        ((eq major-mode 'js-mode) (web-mode) (web-mode-set-content-type "jsx"))
        (t (message "Major mode is neither WEB-MODE nor JS-MODE. Doing nothing"))))
(with-eval-after-load 'web-mode
  (define-key web-mode-map (kbd "C-c C-c C-c") 'mjs/toggle-web-mode))
(with-eval-after-load 'json
  (define-key js-mode-map (kbd "C-c C-c C-c") 'mjs/toggle-web-mode))
