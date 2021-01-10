;;; #lsp
(use-package lsp-mode
  :straight t
  :hook (rust-mode . lsp)
  :bind ("C-c h" . lsp-describe-thing-at-point)
  :custom (lsp-rust-server 'rust-analyzer))
(use-package lsp-ui
  :straight t)

(setq lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
(setq lsp-rust-analyzer-proc-macro-enable t)

;; #rust
(use-package rust-mode
  :straight t
  :custom rust-format-on-save t
  :bind (:map rust-mode-map
              ("M-RET" . lsp-execute-code-action)))

(use-package cargo
  :straight t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :straight t
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))
