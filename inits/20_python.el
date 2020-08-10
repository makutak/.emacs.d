(use-package py-yapf
  :straight t
  :config
  (add-hook 'python-mode-hook 'py-yapf-enable-on-save))

(use-package python-mode
  :straight t
  :config
  (setq python-indent-offset 4))


;; (use-package pipenv
;;   :straight t
;;   :hook (python-mode . pipenv-mode)
;;   :init
;;   (setq
;;    pipenv-projectile-after-switch-function
;;    #'pipenv-projectile-after-switch-extended))

;; require `pip install yapf virtualenv'

(use-package elpy
  :straight t
  :init
  (elpy-enable)
  :config
  (setq elpy-rpc-virtualenv-path 'current))

