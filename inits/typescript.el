(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package typescript-mode)
(use-package tide
  :straight t
  :config
  (setq typescript-indent-level 4)
  (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions
                              t
                              :placeOpenBraceOnNewLineForFunctions
                              nil))
  :after (typescript-mode company flycheck)
  :hook
  (typescript-mode . tide-setup)
  (typescript-mode . tide-hl-identifier-mode)
  (before-save . tide-format-before-save))
