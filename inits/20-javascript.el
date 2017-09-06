(require-or-install 'js2-mode)
(unless (package-installed-p 'js2-mode)
  (package-install 'js2-mode))

(autoload 'js2-mode "js2" nil t)

(eval-after-load "js2-mode"
  '(progn
     (setq js2-basic-offset 2
           js2-mirror-mode nil
           js2-strict-missing-semi-warning nil)))

(unless (package-installed-p 'coffee-mode)
  (package-install 'coffee-mode))
(autoload 'coffee-mode "coffee-mode" nil t)

(require-or-install 'nodejs-repl)
(add-hook 'js2-mode-hook
          (lambda ()
            (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
            (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
            (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
            (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
            (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)))
