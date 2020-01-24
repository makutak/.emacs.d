(use-package python-mode
  :straight t
  :config
  (eldoc-mode t))

(use-package elpy
  :straight t
  :init
  (elpy-enable)
  :config)

;; (use-package py-yapf
;;   :straight t
;;   :hook
;;   (python-mode . py-yapf-enable-on-save))
