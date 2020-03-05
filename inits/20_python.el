(use-package py-yapf
  :straight t)

(use-package python-mode
  :straight t
  :hook
  (python-mode . py-yapf-enable-on-save)
  :config
  (setq python-indent-offset 4))


(use-package pipenv
  :straight t
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))


(use-package elpy
  :straight t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :hook
  (elpy-mode . py-yapf-enable-on-save)
  :config
  (setq elpy-rpc-virtualenv-path 'current)
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

;; (use-package elpy
;;   :straight t
;;   :defer t
;;   :init
;;   (advice-add 'python-mode :before 'elpy-enable))
