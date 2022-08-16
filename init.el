(package-initialize)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")))


;; カラーテーマ
(load-theme 'modus-vivendi)
(setq modus-themes-italic-constructs t
      modus-themes-syntax 'faint)

(set-face-attribute 'default nil
                    :family "DejaVu Sans Mono"
                    :height 120)


;; キーバインド
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(define-key global-map (kbd "C-m") 'newline-and-indent)
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)
(define-key global-map (kbd "C-t") 'other-window)
(define-key global-map (kbd "C-x ?") 'help-command)

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(orderless marginalia package-utils consult)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
