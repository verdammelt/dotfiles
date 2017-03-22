(use-package elixir-mode
  :config (add-hook 'elixir-mode-hook 'alchemist-mode))
(use-package alchemist
  :diminish (alchemist-mode alchemist-phoenix-mode))
(use-package elixir-yasnippets)
(use-package flycheck-elixir)
(use-package flycheck-credo
  :config (setq flycheck-elixir-credo-strict t))
