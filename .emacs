(setq user-emacs-directory (concat "~/.emacs.d." (int-to-string emacs-major-version) "/"))
(load-file (concat user-emacs-directory "/init.el"))

