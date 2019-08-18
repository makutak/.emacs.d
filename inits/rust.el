(use-package rust-mode
  :defer t
  :config
  (setq rust-format-on-save t))

(use-package racer
  :init
  (add-hook 'rust-mode #'racer-mode)
  (add-hook 'racer-mode #'eldoc-mode))

(use-package flycheck-rust
  :init
  (add-hook 'rust-mode-hook
            '(lambda ()
               (flycheck-mode)
               (flycheck-rust-setup))))
