(use-package company
  :config
  (global-company-mode)
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-dabbrev-downcase nil)

  (bind-keys ("TAB" . company-indent-or-complete-common))
  (bind-keys :map company-mode-map
             ("C-i" . company-complete)
             ("TAB" . nil))
  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-s" . company-search-words-regexp)
             ("TAB" . company-complete-common-or-cycle))
  (bind-keys :map company-search-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)))
