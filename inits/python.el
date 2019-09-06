(use-package python-mode
  :straight t
  :config
  (eldoc-mode t))

(use-package elpy
  :straight t
  :init
  (elpy-enable)
  :config
  (setq python-shell-interpreter "ipython"
        python-shell-interpreter-args "--simple-prompt -i"))

(use-package py-yapf
  :straight t
  :hook
  (python-mode . py-yapf-enable-on-save))
