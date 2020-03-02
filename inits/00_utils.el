(use-package company
  :straight t
  :init (global-company-mode)
  :bind (("TAB" . company-indent-or-complete-common)
         :map company-mode-map
         ("C-i" . company-complete)
         ("TAB" . nil)
         :map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)
         ("C-s" . company-search-words-regexp)
         ("TAB" . company-complete-common-or-cycle)
         :map company-search-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 2
        company-selection-wrap-around t
        company-dabbrev-downcase nil)
  (push 'company-lsp company-backends))

(use-package flycheck
  :straight t
  :init (global-flycheck-mode)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package magit)

(use-package rainbow-delimiters
  :straight t
  :hook ((emacs-lisp-mode . rainbow-delimiters-mode)
         (eval-expression-minibuffer-setup-hook . rainbow-delimiters-mode)
         (ielm-mode-hook . rainbow-delimiters-mode)
         (lisp-mode-hook . rainbow-delimiters-mode)
         (lisp-interaction-mode-hook . rainbow-delimiters-mode)
         (scheme-mode-hook . rainbow-delimiters-mode)
         (clojure-mode-hook . rainbow-delimiters-mode)))

(use-package paredit
  :straight t
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (eval-expression-minibuffer-setup-hook . enable-paredit-mode)
         (ielm-mode-hook . enable-paredit-mode)
         (lisp-mode-hook . enable-paredit-mode)
         (lisp-interaction-mode-hook . enable-paredit-mode)
         (scheme-mode-hook . enable-paredit-mode)))


(use-package eldoc
  :straight t
  :hook ((emacs-lisp-mode-hook . turn-on-eldoc-mode)
         (lisp-interaction-mode-hook . turn-on-eldoc-mode)
         (ielm-mode-hook . turn-on-eldoc-mode)))


;; copy to clipboard
(setq x-select-enable-clipboard t)
(use-package xclip
  :config (xclip-mode 1))
