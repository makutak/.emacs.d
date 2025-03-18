;;; Personal configuration -*- lexical-binding: t -*-

;; パッケージ管理
(eval-and-compile
  (require 'package)
  (setq package-archives
        '(("gnu"   . "https://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/")
          ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))

;; `use-package` のデフォルト設定
(setq use-package-always-ensure t)

;; `go-mode` の設定
(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . lsp-deferred)
  :custom (gofmt-command "goimports")
  :config
  (setq tab-width 4)
  (add-hook 'before-save-hook #'gofmt-before-save))

;; `lsp-mode` の設定
(use-package lsp-mode
  :commands lsp
  :hook ((go-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (rust-mode . lsp-deferred))
  :custom
  (lsp-completion-provider :none)
  (lsp-enable-snippet nil)
  (lsp-go-gopls-server-args '("-remote=auto"))
  (lsp-session-folders-remove '("/usr/local/go/src" "~/go/pkg/mod")))

;; `lsp-ui` (補助UI)
(use-package lsp-ui
  :after lsp-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-sideline-enable t))

;; `corfu` (補完機能)
(use-package corfu
  :bind (:map corfu-map
              ("TAB" . corfu-next)
              ("S-TAB" . corfu-previous))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  :init
  (global-corfu-mode))

;; `magit` (Git クライアント)
(use-package magit
  :bind ("C-x g" . magit-status))

;; `vertico` (ミニバッファ補完)
(use-package vertico
  :init
  (vertico-mode))

;; `marginalia`（補足情報を表示）
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; `orderless`（部分一致検索）
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless))  ;; `orderless` を Emacs の補完スタイルに設定
  (completion-category-overrides '((file (styles basic)))))  ;; `find-file` では通常の補完を使う

;; `consult`（高度な検索＆ナビゲーション）
(use-package consult
  :ensure t
  :bind
  (("C-s" . consult-line)  ;; `swiper` の代替
   ("M-y" . consult-yank-pop)  ;; `M-y`（履歴ペースト）を強化
   ("C-x b" . consult-buffer)  ;; `C-x b`（バッファ切り替え）を強化
   ("C-x C-r" . consult-recent-file)))  ;; `C-x C-r` で最近開いたファイル一覧


;; `flymake` (静的解析)
(use-package flymake
  :hook ((go-mode . flymake-mode)
         (python-mode . flymake-mode)
         (rust-mode . flymake-mode)))

;;; UI の調整（`use-package` 不要）
(load-theme 'tsdh-dark)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen t)
(global-display-line-numbers-mode t)
(electric-pair-mode t)
(set-face-attribute 'default nil :font "Ricty-14")

;;; キーバインド設定（`use-package` 不要）
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-t") 'other-window)

;; バックアップ・オートセーブの無効化
(setq make-backup-files nil
      auto-save-default nil)

;; `clang-format` の設定 (C/C++)
(use-package clang-format
  :custom (clang-format-style "google")
  :hook ((c-mode . my-clang-format-before-save)
         (c++-mode . my-clang-format-before-save))
  :config
  (defun my-clang-format-before-save ()
    (interactive)
    (when (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
      (clang-format-buffer))))

;; `rust-mode`
(use-package rust-mode
  :hook (rust-mode . (lambda () (add-hook 'before-save-hook 'rust-format-buffer nil 'local))))

;; `multiple-cursors`
(use-package multiple-cursors
  :bind ("M-D" . mc/mark-next-like-this))

;; `iedit`
(use-package iedit)

;; `whitespace` 設定
(use-package whitespace
  :custom
  (whitespace-style '(face empty tabs lines-tail trailing))
  :hook (before-save . delete-trailing-whitespace)
  :init
  (global-whitespace-mode t))

;; `recentf-mode` を有効化
(use-package recentf
  :init
  (recentf-mode t))

;; `yaml-mode`
(use-package yaml-mode)

;; `json-mode`
(use-package json-mode)

;; `typescript-mode`
(use-package typescript-mode)

;; `auctex` (LaTeX)
(use-package auctex
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-master nil))

;; `markdown-mode`
(use-package markdown-mode)

;; `brief` (アウトラインベースのノート管理)
(use-package brief)

(use-package smartparens
  :ensure t
  :hook ((emacs-lisp-mode . smartparens-mode)
         (lisp-mode . smartparens-mode)
         (lisp-interaction-mode . smartparens-mode)
         (scheme-mode . smartparens-mode)
         (common-lisp-mode . smartparens-mode))
  :config
  (require 'smartparens-config)
  ;; `C-→` (`Control + →`) で slurp
  (define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
  ;; `C-←` (`Control + ←`) で barf
  (define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp))


(defun my-lisp-auto-format ()
  "LISP 系のファイルを保存するときに自動フォーマットする。"
  (when (derived-mode-p 'emacs-lisp-mode 'lisp-mode 'scheme-mode 'common-lisp-mode)
    (indent-region (point-min) (point-max))))

(add-hook 'before-save-hook #'my-lisp-auto-format)

(when (getenv "DISPLAY")  ;; GUI 環境なら
  (use-package xclip
    :ensure t
    :config
    (xclip-mode 1)))

;; `company-mode` の C 言語用設定
(use-package company
  :ensure t
  :hook (c-mode . company-mode)
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-selection-wrap-around t
        company-tooltip-align-annotations t)
  (setq company-backends
        '((company-clang company-dabbrev-code company-keywords))))

;; `company-clang` の設定
(setq company-clang-arguments '("-std=c11" "-I/usr/include" "-I./include"))

;; `company-box` で補完ウィンドウを改善
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))


;; Emacs のデフォルト設定改善
(setq large-file-warning-threshold 100000000) ;; 100MB 以上のファイル警告
(defalias 'yes-or-no #'y-or-n-p)

;; Emacs 起動時にウィンドウを最大化
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(consult orderless marginalia smartparens keybindings ui-settings yaml-mode vertico typescript-mode rust-mode multiple-cursors magit lsp-ui leaf-keywords json-mode iedit hydra go-mode el-get corfu clang-format brief blackout auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
