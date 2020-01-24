(add-to-list 'exec-path "~/work/kotlin-language-server/server/build/install/server/bin")

(use-package kotlin-mode)
(use-package flycheck-kotlin
	:after kotlin-mode
	:init
	(flycheck-kotlin-setup))

(use-package lsp-mode
  :hook (kotlin-mode . lsp)
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
(use-package dap-mode)
