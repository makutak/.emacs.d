(use-package lsp-mode
  :straight t
  :hook
  (kotlin-mode . lsp)
  :commands lsp)

;; optionally
(use-package lsp-ui
  :disabled t)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :disabled t)

;; optionally if you want to use debugger
(use-package dap-mode
  :disabled t)
