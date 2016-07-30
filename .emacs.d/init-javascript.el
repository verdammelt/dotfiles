(require 'nvm)
(nvm-use (caar (last (nvm--installed-versions)))) ; cavalierly use first one
(add-to-list 'exec-path (getenv "NVM_BIN"))

(with-eval-after-load 'projectile
  (add-hook 'projectile-after-switch-project-hook 'mjs/add-node-modules-in-path))

(defvar mjs/project-node-module-special-cases (list))
(add-to-list 'mjs/project-node-module-special-cases "VestaWeb")

(defvar mjs/previous-node-modules-added-to-path nil)

(defun mjs/add-node-modules-in-path ()
  "I don't install project dependencies globally so I need to add
the .node_modules/.bin directory to the exec path. Sometimes the
node_modules directory is not in the project root, add special
case subdirectory names to
mjs/project-node-module-special-cases."
  (interactive)
  (let* ((all-possibilities
          (mapcar #'(lambda (dir) (expand-file-name "./node_modules/.bin" dir))
                  (cons "./" mjs/project-node-module-special-cases)))
         (node-modules-bind-dir
          (cl-find-if #'file-exists-p all-possibilities)))
    (when mjs/previous-node-modules-added-to-path
      (setq exec-path
            (cl-remove mjs/previous-node-modules-added-to-path exec-path
                       :test #'string-equal)
            mjs/previous-node-modules-added-to-path nil))
    (when node-modules-bind-dir
      (setq mjs/previous-node-modules-added-to-path node-modules-bind-dir)
      (cl-pushnew node-modules-bind-dir exec-path))))

(add-to-list 'auto-mode-alist '("\\.jsx?$" . js2-jsx-mode))
(with-eval-after-load 'js2-mode
  (defvar sgml-basic-offset)
  (defvar sgml-attribute-offset)
  (defvar js-indent-level)
  (diminish 'js2-mode "JS")
  (diminish 'js2-jsx-mode "JSX")
  (setq js-indent-level 2)
  (setq sgml-basic-offset js-indent-level
        sgml-attribute-offset js-indent-level))

(defun mjs/toggle-indent-level ()
  "Some projects use 4 spaces, some use 2."
  (interactive)
  (defvar js-indent-level)
  (defvar sgml-attribute-offset)
  (defvar sgml-basic-offset)
  (let ((offset (if (= js-indent-level 2) 4 2)))
    (setq js-indent-level offset
          sgml-basic-offset offset
          sgml-attribute-offset offset)))

(with-eval-after-load 'typescript
  (add-hook 'typescript-mode-hook 'tide-setup))
