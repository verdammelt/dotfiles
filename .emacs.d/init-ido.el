;;;;
;;;; IDO
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Time-stamp: <2014-04-30 14:46:42 mjs>
;;;;
(after 'ido
  (setq ido-show-dot-for-dired t
	ido-enable-flex-matching t)
  (ido-vertical-mode)
  (ido-sort-mtime-mode))

(provide 'init-ido)
