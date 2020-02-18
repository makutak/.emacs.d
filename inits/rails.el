(use-package projectile-rails
  :disabled t
  :commands (projectile-rails-on)
  :init
  (add-hook 'projectile-mode-hook 'projectile-rails-on))
