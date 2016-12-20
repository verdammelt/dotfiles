(autoload 'nvm-use "nvm")
(autoload 'nvm-use-for "nvm")
(autoload 'nvm--installed-versions "nvm")

(defvar mjs/previous-node-version nil)
(defun mjs/choose-node-version ()
  "Choose and use the correct version of node for this current
directory. If there is a .nvmrc file use that - otherwise pick
one of the installed versions (arbitrarily: the last)."
  (when mjs/previous-node-version
    (message "removing old node version: %s" mjs/previous-node-version)
    (setq exec-path
          (cl-remove mjs/previous-node-version exec-path
                     :test #'string=)
          mjs/previous-node-version nil))
  (if (file-exists-p ".nvmrc")
      (nvm-use-for ".")
    (nvm-use (caar (last (nvm--installed-versions)))))

  (message "selecting new node-version: %s" (getenv "NVM_BIN"))
  (setq mjs/previous-node-version (getenv "NVM_BIN")
        exec-path (cl-pushnew mjs/previous-node-version exec-path
                              :test #'string=)))

(defvar mjs/project-node-module-special-cases (list)
  "Some projects may not have their node_modules directory at
  their top level.")
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
      (message "removing old node-modules path: %s"
               mjs/previous-node-modules-added-to-path)
      (setq exec-path
            (cl-remove mjs/previous-node-modules-added-to-path exec-path
                       :test #'string=)
            mjs/previous-node-modules-added-to-path nil))
    (when node-modules-bind-dir
      (message "adding new node-modules path: %s" node-modules-bind-dir)
      (setq mjs/previous-node-modules-added-to-path node-modules-bind-dir
            exec-path (cl-pushnew node-modules-bind-dir exec-path
                                  :test #'string=)))))


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
  (defvar typescript-indent-level)
  (defvar sgml-attribute-offset)
  (defvar sgml-basic-offset)
  (let ((offset (if (= js-indent-level 2) 4 2)))
    (setq js-indent-level offset
          typescript-indent-level offset
          sgml-basic-offset offset
          sgml-attribute-offset offset)))

(with-eval-after-load 'typescript-mode
  (defvar typescript-indent-level)
  (setq typescript-indent-level 2)
  (defun setup-tide ()
    (tide-setup)
    (eldoc-mode +1))
  (add-hook 'typescript-mode-hook 'setup-tide))

(with-eval-after-load 'projectile
  (add-hook 'projectile-after-switch-project-hook 'mjs/add-node-modules-in-path)
  (add-hook 'projectile-after-switch-project-hook 'mjs/choose-node-version))

(mjs/choose-node-version)
