;;;;
;;;; IDO
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
;;;; Time-stamp: <2014-01-22 20:10:04 mark>
;;;;
(after 'ido
  (setq ido-show-dot-for-dired t
	ido-enable-flex-matching t)

  (ido-vertical-mode)
  (ido-sort-mtime-mode))

(provide 'init-ido)
