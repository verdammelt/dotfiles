(declare-function mjs/set-path-envvar-from-exec-path "init")

(defun mjs/js-mode-setup () (setq mode-name "JS"))
(use-package js
  :ensure nil
  :config
  (setq js-indent-level 2)
  (add-hook 'js-mode-hook 'mjs/js-mode-setup))

(use-package nvm
  :commands (nvm-use nvm-use-for nvm--installed-versions))

(use-package prettier-js
  :diminish (prettier-js-mode "Pr")
  :hook ((js-mode typescript-mode typescriptreact-mode web-mode) . prettier-js-mode))

(defun mjs/add-to-path (dir)
  (cl-pushnew dir exec-path :test #'string=)
  (mjs/set-path-envvar-from-exec-path))

(defun mjs/remove-from-path (dir)
  (setq exec-path (cl-remove dir exec-path :test #'string=))
  (mjs/set-path-envvar-from-exec-path))

(defun mjs/remove-node-from-path ()
  (mjs/remove-from-path (file-name-as-directory (getenv "NVM_BIN"))))

(defun mjs/add-node-to-path ()
  (cond ((file-exists-p ".nvmrc")
         (nvm-use-for "."))
        ((nvm--installed-versions)
         (nvm-use (car (first (cl-sort (nvm--installed-versions)
                                       #'string< :key #'first))))))
  (mjs/set-path-envvar-from-exec-path))

(defun mjs/remove-node-modules-from-path ()
  "Remove current node_module/.bin directory from the path"
  (when-let ((node-module-bin
              (locate-dominating-file default-directory "node_modules/.bin/")))
    (mjs/remove-from-path (expand-file-name "node_modules/.bin/" node-module-bin))))

(defun mjs/add-node-modules-in-path ()
  "Add node_modules/.bin to exec-path and PATH if there is one in this project."
  (when-let ((node-module-bin
              (locate-dominating-file default-directory "node_modules/.bin/")))
    (mjs/add-to-path (expand-file-name "node_modules/.bin/" node-module-bin))))

(defun mjs/project-teardown-js-env ()
  (when mjs/current-project
    (let ((default-directory (project-root mjs/current-project)))
      (mjs/remove-node-modules-from-path)
      (mjs/remove-node-from-path))))

(defun mjs/project-setup-js-env ()
  (when mjs/current-project
    (let ((default-directory (project-root mjs/current-project)))
      (mjs/add-node-to-path)
      (mjs/add-node-modules-in-path))))

(add-hook 'mjs/project-teardown-hook 'mjs/project-teardown-js-env)
(add-hook 'mjs/project-setup-hook 'mjs/project-setup-js-env)
