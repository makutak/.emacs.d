(setq exec-path (cons (expand-file-name "~/.local/bin") exec-path))
(setq exec-path (cons (expand-file-name "~/.cargo/bin") exec-path))

(use-package rust-mode
  :ensure t
  :custom rust-format-on-save t)


(use-package cargo
  :ensure t
  :hook (rust-mode . cargo-minor-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; #lsp

(use-package lsp-mode
  :ensure t
  :hook (rust-mode . lsp)
  :bind ("C-c h" . lsp-describe-thing-at-point)
  :custom (lsp-rust-server 'rust-analyzer))
(use-package lsp-ui
  :ensure t)

(setq lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
(setq lsp-rust-analyzer-proc-macro-enable t)
