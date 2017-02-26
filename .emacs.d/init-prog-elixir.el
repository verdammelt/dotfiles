(use-package elixir-mode
  :defer t
  :config (add-hook 'elixir-mode-hook 'alchemist-mode))

(use-package alchemist
  :defer t
  :diminish (alchemist-mode alchemist-phoenix-mode))

(use-package elixir-yasnippets :defer t)

(use-package flycheck-elixir
  :defer t
  :init (setq flycheck-elixir-credo-strict t))

(use-package flycheck-credo :defer t)
