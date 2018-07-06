(declare-function mjs/set-path-envvar-from-exec-path "init")

(use-package nvm
  :commands (nvm-use nvm-use-for nvm--installed-versions))

(use-package js2-mode
  :config
  (setq js2-basic-offset 2))

(defvar mjs/previous-node-version nil)
(defun mjs/remove-node-from-path ()
  (message "Removing %s from path" mjs/previous-node-version)
  (setq exec-path
        (cl-remove mjs/previous-node-version exec-path :test #'string=)
        mjs/previous-node-version nil)
  (mjs/set-path-envvar-from-exec-path))

(defun mjs/add-node-to-path ()
  (if (file-exists-p ".nvmrc")
      (nvm-use-for ".")
    (nvm-use (caar (last (nvm--installed-versions)))))
  (setq mjs/previous-node-version (getenv "NVM_BIN")
        exec-path (cl-pushnew mjs/previous-node-version exec-path
                              :test #'string=))
  (mjs/set-path-envvar-from-exec-path)
  (message "Added %s to path" mjs/previous-node-version))

(defvar mjs/project-node-module-special-cases (list)
  "Some projects may not have their node_modules directory at
  their top level. (Or may have additional node_modules that need
  to be checked for as well.")
(defvar mjs/previous-node-modules-added-to-path nil)
(defun mjs/remove-node-modules-from-path ()
  (message "Removed %s from path" mjs/previous-node-modules-added-to-path)
  (setq exec-path
        (cl-remove-if
         #'(lambda (p) (member p mjs/previous-node-modules-added-to-path))
         exec-path)
        mjs/previous-node-modules-added-to-path nil)
  (mjs/set-path-envvar-from-exec-path))

(defun mjs/add-node-modules-in-path ()
  "I don't install project dependencies globally so I need to add
the .node_modules/.bin directory to the exec path. Sometimes the
node_modules directory is not in the project root, add special
case subdirectory names to mjs/project-node-module-special-cases."
  (let* ((all-possibilities
          (mapcar #'(lambda (dir) (expand-file-name "./node_modules/.bin" dir))
                  (cons "./" mjs/project-node-module-special-cases)))
         (node-modules-bin-dirs
          (cl-remove-if-not #'file-exists-p all-possibilities)))
    (when node-modules-bin-dirs
      (setq mjs/previous-node-modules-added-to-path node-modules-bin-dirs
            exec-path (cl-remove-duplicates
                       (append node-modules-bin-dirs exec-path)))
      (mjs/set-path-envvar-from-exec-path))
    (message "Added %s to path" mjs/previous-node-modules-added-to-path)))
