(use-package typescript-mode
  :mode "\\.tsx\\'"
  :config
  (setq typescript-indent-level 4))

(define-auto-insert '("\\.tsx" . "TSX Component")
  '((read-string "Component name: " nil nil (file-name-base (buffer-file-name)))
    "import React from 'react';" \n \n
    "export const " str " = () => <div>blah</div>;" \n \n))

(define-auto-insert '("\\.test\\.tsx" . "TSX Component test")
  '((read-string "Component name: " nil nil (file-name-base (file-name-base (buffer-file-name))))
    "import { " str  " } from '../" str "';" \n \n
    "describe('" str "', () => {" \n
    > "test('fails', () => expect(true).toBe(false))" \n \n
    "})" \n))
