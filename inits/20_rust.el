(use-package rustic
  :straight t
  :config
  (yas-minor-mode 1)
  (setq rustic-lsp-server 'rls)
  (setq rustic-format-trigger 'on-save))
