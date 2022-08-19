(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;; coding
(setq set-language-environment "Japanese")
(setq prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(setq set-default-coding-systems 'utf-8)
(setq set-terminal-coding-system 'utf-8)
(setq set-keyboard-coding-system 'utf-8)
(setq set-clipboard-coding-system 'utf-8)
(setq set-buffer-file-coding-system 'utf-8-unix)
(setq file-name-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; yes or no -> y or n
(fset 'yes-or-no-p 'y-or-n-p)

;; カラーテーマ
(load-theme 'modus-vivendi)
(setq modus-themes-italic-constructs t
      modus-themes-syntax 'faint)

(set-face-attribute 'default nil
                    :family "Ricty Diminished"
                    :height 140)

;; キーバインド
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(define-key global-map (kbd "C-m") 'newline-and-indent)
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
(define-key global-map (kbd "C-t") 'other-window)
(define-key global-map (kbd "C-x ?") 'help-command)

;; ツールバーをなくす
(tool-bar-mode -1)
;; メニューバーをなくす
(menu-bar-mode -1)
;; スクロールは１行ごとに
(setq scroll-conservatively 1)
;; 文末の空行を削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; 起動時のスプラッシュ画面を表示しない
(setq inhibit-splash-screen t)
;; スタートアップ画面を表示しない
(setq inhibit-startup-message t)

;; dired
(setq dired-auto-revert-buffer t
      dired-listing-switches "-alh")

;; 行番号を表示
(global-display-line-numbers-mode)

;; ヘッダにfilename
(setq frame-title-format "%f")

;; Tab
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; 現在行をハイライト
(global-hl-line-mode t)

;; paren
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)
(electric-pair-mode t)

;; バックアップとオートセーブを無効
(setq make-backup-files nil)
(setq auto-save-default nil)

;; 更新されたファイルを自動的に読み込み直す
(global-auto-revert-mode t)

;; 補完UI
(savehist-mode t)
(setq vertico-count 20)
(fido-vertical-mode t)

(require 'consult)
(define-key global-map (kbd "C-x b") 'consult-buffer)
(define-key global-map [remap goto-line] 'consult-goto-line)

(require 'marginalia)
(marginalia-mode t)

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-overrides '((file (styles basic partial-completion))))

(require 'company)
(global-company-mode t)
(setq company-idle-delay 0
      company-echo-delay 0
      company-minimum-prefix-length 2
      company-selection-wrap-around t
      company-show-numbers t)

(define-key company-active-map (kbd "C-n") 'company-select-next)
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map [tab] 'company-complete-selection)
(define-key company-active-map (kbd "C-h") nil)

(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)

(require 'lsp-mode)
(setq lsp-enable-snippet nil)
(add-hook 'c-mode-hook #'lsp)

(require 'lsp-ui)
(add-hook 'lsp-mode-hook 'lsp-ui-mode)
(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
(setq lsp-ui-doc-enable t
      lsp-ui-doc-position 'top
      lsp-ui-doc-alignment 'window
      lsp-ui-doc-header t
      lsp-ui-peek-enable t
      lsp-ui-doc-include-signature t
      lsp-ui-peek-peek-height 20
      lsp-ui-peek-list-width 50)

(require 'lua-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lua-mode lsp-ui lsp-mode company orderless marginalia package-utils consult)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
