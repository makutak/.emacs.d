(package-initialize)

(use-package python-mode
  :straight t)

(use-package elpy
  :straight t
  ;;:ensure t
  :defer t
  :init
  (elpy-enable))
