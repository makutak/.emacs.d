(use-package typescript-mode
  :config
  (setq typescript-indent-level 2))

(use-package rjsx-mode
  :mode ("\\.tsx" "\\.js")
  :config
  (setq indent-tabs-mode nil)
  (setq js-indent-level 2))
(use-package lsp-mode
  :hook ((typescript-mode rjsx-mode) . lsp)
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
