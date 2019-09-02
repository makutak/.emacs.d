(use-package rust-mode
  :defer t
  :config
  (setq rust-format-on-save t))

(use-package racer
  :init
  :hook ((rust-mode . racer-mode))
  :config
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode)
  (define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
  (setq company-tooltip-align-annotations t))


(use-package flycheck-rust
  :init
  (add-hook 'rust-mode-hook
            '(lambda ()
               (flycheck-mode)
               (flycheck-rust-setup))))
