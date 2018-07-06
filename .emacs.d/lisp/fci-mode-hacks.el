;;;
;;; snarfed from https://github.com/alpaker/Fill-Column-Indicator/issues/46#issuecomment-371712873
;;;
(defvar-local mjs/fci-mode-stack '()
  "track fci-mode state to aid advice functions.")

(defun mjs/fci-conditional-enable (&rest _)
  "Conditionally (re-)enable fci-mode."
  (when (eq (pop mjs/fci-mode-stack) t)
    (fci-mode t)))

(defun mjs/fci-get-and-disable (&rest _)
  "Store current status of fci-mode, and disable if needed."
  (when (boundp 'fci-mode)
    (push fci-mode mjs/fci-mode-stack)
    (when fci-mode
      (fci-mode -1))))

(defun mjs/fci-hack (advised-func &rest args)
  "Disable fci-mode, call ADVISED-FUNC with ARGS, then re-enable fci-mode."
  (progn
    (mjs/fci-get-and-disable)
    (apply advised-func args)
    (mjs/fci-conditional-enable)))

;;;
;;; Then use like so:
;;; (these are placed near my setup of these modes)
;;;
;;
;; ;; disable fci-mode while certain operations are being performed
;; (advice-add 'web-mode-on-after-change :around #'fci-hack)
;; (advice-add 'web-mode-on-post-command :around #'fci-hack)
;; (add-hook 'company-completion-started-hook 'fci-get-and-disable)
;; (add-hook 'company-completion-cancelled-hook 'fci-conditional-enable)
;; (add-hook 'company-completion-finished-hook 'fci-conditional-enable)
;;

(provide 'fci-mode-hacks)
