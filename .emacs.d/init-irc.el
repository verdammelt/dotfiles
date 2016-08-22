(load-init-file "init-secrets")

(defun mjs/cyrus-erc ()
  (interactive)
  (apply #'erc-tls mjs/cyrus-irc-info))

(defun mjs/slack-erc ()
  (interactive)
  (mjs/cyrus-erc))

(with-eval-after-load 'erc
  (add-hook 'erc-mode-hook 'turn-on-flyspell)
  (setq erc-prompt #'(lambda () (concat "[" (buffer-name) "]")))

  ;; nasty hack because gary is not showing up as a user in the channel.
  (setq erc-format-nick-function
        #'(lambda (&optional user channel-data)
            (if user (erc-format-nick user channel-data)
              (progn (message "missing user - assuming gary")
                     "gary"))))

  (add-to-list 'erc-modules 'smiley)
  (add-to-list 'erc-modules 'image)
  (add-to-list 'erc-modules 'youtube)
  )


(with-eval-after-load 'erc-track
  (add-to-list 'erc-track-exclude-types "MODE"))

(with-eval-after-load 'erc-image
  (setq erc-image-inline-rescale 300))
