(use-package typescript-mode)
(use-package js2-mode
  :mode "\\.js\\'")

(use-package lsp-mode
  :hook ((typescript-mode js2-mode) . lsp)
  :commands lsp)

;; optionally
(use-package lsp-ui
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-flycheck-enable t
        lsp-ui-peek-enable t
        lsp-ui-sideline-enable nil
        lsp-ui-doc-position 'at-point))

(use-package company-lsp :commands company-lsp)
;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
(use-package dap-mode)
