(use-package json-mode
  :straight t
  :hook
  (json-mode . json-mode-hook)
  :config
  (setq js-indent-level 2))
