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

(defun mjs/setup-local-eslint ()
    "If ESLint found in node_modules directory - use that for flycheck.
Intended for use in PROJECTILE-AFTER-SWITCH-PROJECT-HOOK."
    (interactive)
    (let ((local-eslint (expand-file-name "./node_modules/.bin/eslint")))
      (defvar flycheck-javascript-eslint-executable)
      (setq flycheck-javascript-eslint-executable
            (and (file-exists-p local-eslint) local-eslint))))

;;;
;;; will try built-in js-jsx-mode and see if that works well
;;;
;;; Since I don't see a clear winner between naming React/ReactNative
;;; files *.js or *.jsx I will use web-mode for both.
;;;
(add-to-list 'auto-mode-alist '("\\.jsx?$" . js2-jsx-mode))
(with-eval-after-load 'js2-mode
  (defvar sgml-basic-offset)
  (defvar sgml-attribute-offset)
  (defvar js-indent-level)
  (diminish 'js2-mode "JS")
  (diminish 'js2-jsx-mode "JSX")
  (setq sgml-basic-offset js-indent-level
        sgml-attribute-offset js-indent-level))

;;;
;;; Javascript mode is used for JSON files. I'd like its identation to
;;; match the code.
;;;
(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
