(require-or-install 'flycheck)
(require-or-install 'typescript-mode)
(unless (package-installed-p 'tide)
  (package-install 'tide))
(require-or-install 'tide)

(unless (package-installed-p 'company)
  (package-install 'company))
(require-or-install 'company)
(setq tide-tsserver-executable "~/.anyenv/envs/ndenv/shims/tsserver")
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

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

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
