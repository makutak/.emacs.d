(use-package python-mode
  :straight t
  :config
  (eldoc-mode t)
  (setq python-indent-offset 4))

(use-package elpy
  :straight t
  ;;:ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (setq elpy-rpc-virtualenv-path 'current)
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

;; (use-package elpy
;;   :straight t
;;   :defer t
;;   :ensure t
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable))
