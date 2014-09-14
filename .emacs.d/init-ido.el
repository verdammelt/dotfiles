;;;;
;;;; IDO
;;;;
;;;; [if found please return to damned@theworld.com]
;;;;
(after 'ido
  (setq ido-show-dot-for-dired t
	ido-enable-flex-matching t)
  (ido-vertical-mode)
  (ido-sort-mtime-mode))
