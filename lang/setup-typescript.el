(use-package tide
  :straight t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

  ;; (setq typescript-indent-level 4)
  ;; (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions
  ;;                           t
  ;;                           :placeOpenBraceOnNewLineForFunctions
  ;;                           nil))
