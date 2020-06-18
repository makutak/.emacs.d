(use-package dockerfile-mode
  :straight t
  :mode (("Dockerfile\\'" . dockerfile-mode)))

(use-package docker-compose-mode
  :straight t)

(use-package docker-tramp
  :straight t
  :config
  (set-variable 'docker-tramp-use-names t))

