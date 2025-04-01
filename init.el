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

(defun my-clang-format-before-save ()
  "C/C++ の保存時に clang-format を自動実行"
  (when (or (eq major-mode 'c-mode)
            (eq major-mode 'c++-mode))
    (condition-case err
        (clang-format-buffer)
      (error (message "clang-format error: %s" err)))))

(use-package clang-format
  :ensure t
  :hook ((c-mode . (lambda ()
                     (add-hook 'before-save-hook #'my-clang-format-before-save nil t)))
         (c++-mode . (lambda ()
                       (add-hook 'before-save-hook #'my-clang-format-before-save nil t))))
  :custom
  (clang-format-style "file"))  ;; .clang-format を参照


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

;; ripgrep + deadgrep
(use-package deadgrep
  :ensure t
  :bind (("C-c s" . deadgrep)))

;; dumb-jump を xref に統合（fallback的に）
(use-package dumb-jump
  :ensure t
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-aggressive nil)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; xref を使った統一ジャンプキー
(global-set-key (kbd "M-.") #'xref-find-definitions)
(global-set-key (kbd "M-,") #'xref-pop-marker-stack)

;; TAGS 読み込み（etags）
;; TAGSファイルは .dir-locals.el 側で tags-table-list を指定する
;; init.el 側では、TAGS 機能の自動追加を無効にするだけでOK
(setq tags-add-table nil)

;; cscope
(use-package xcscope
  :ensure t
  :init
  (cscope-setup))

(defun my/update-tags-and-cscope ()
  "保存時に make update を実行して TAGS / cscope.out を再生成する。"
  (when (and buffer-file-name
             (string-match "\\.c\\|\\.h$" buffer-file-name)
             (locate-dominating-file buffer-file-name "Makefile"))
    (let ((default-directory (locate-dominating-file buffer-file-name "Makefile")))
      (when (and (executable-find "make")
                 (file-exists-p "Makefile"))
        (shell-command "make update")))))

(add-hook 'after-save-hook #'my/update-tags-and-cscope)

;; man
(global-set-key (kbd "C-c m") 'man)

(use-package org
  :ensure t
  :pin gnu  ;; ELPA の GNU リポジトリから取得
  :config
  (setq org-hide-leading-stars t)   ;; ヘッダの * を非表示
  (setq org-startup-indented t)     ;; インデント表示を有効化
  (setq org-pretty-entities t)      ;; 数式や記号を綺麗に表示
  (setq org-log-done 'time)         ;; タスク完了時に時間を記録
  (setq org-return-follows-link t)) ;; RETキーでリンクを開く

(setq org-agenda-files '("~/org")) ;; `~/org/` フォルダ内のファイルを管理対象にする
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-default-notes-file "~/org/notes.org")  ;; メモの保存先
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
         "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
        ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
         "* %?\n  %U\n  %a\n  %i" :empty-lines 1)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit default :weight bold :height 1.5))))
 '(org-level-2 ((t (:inherit default :weight bold :height 1.3))))
 '(org-level-3 ((t (:inherit default :weight bold :height 1.2))))
 '(org-level-4 ((t (:inherit default :weight bold :height 1.1)))))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "~/org-roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph))
  :init
  (org-roam-db-autosync-mode))

(use-package asm-mode
  :mode ("\\.s\\'" . asm-mode)
  :hook (asm-mode . (lambda ()
                      (setq tab-width 2)
                      (setq indent-tabs-mode nil)
                      (setq asm-indent-level 2))))

(use-package vterm
  :ensure t)

;; Emacs のデフォルト設定改善
(setq large-file-warning-threshold 100000000) ;; 100MB 以上のファイル警告
(defalias 'yes-or-no #'y-or-n-p)
(global-set-key (kbd "C-x =") #'balance-windows)


(defun my/replace-commas-with-newlines (start end)
  "Replace all commas with newlines in the region from START to END.
If no region is active, apply to the entire buffer."
  (interactive "r")
  (if (use-region-p)
      ;; リージョンが選択されている場合はその範囲に適用
      (perform-replace "," "\n" nil nil nil nil nil start end)
    ;; リージョンが選択されていない場合はバッファ全体に適用
    (perform-replace "," "\n" nil nil nil nil nil (point-min) (point-max))))
(global-set-key (kbd "C-c ,") #'my/replace-commas-with-newlines)

(add-hook 'server-visit-hook
          (lambda ()
            (setq default-directory
                  (expand-file-name (getenv "PWD")))))

;; フォント設定関数（システムに応じてフォントサイズを変える）
(defun my/set-default-font ()
  (let ((font-size (cond
                    ((eq system-type 'darwin) 18)
                    ((eq system-type 'gnu/linux) 14)
                    (t 14)))  ;; fallback
        (font-name "Ricty"))
    (set-face-attribute 'default nil :font (format "%s-%d" font-name font-size))))

;; daemon時：GUIフレームが作られるたびに適用
(defun my/set-font-for-new-frame (frame)
  (with-selected-frame frame
    (when (display-graphic-p frame)
      (my/set-default-font))))

(add-hook 'after-make-frame-functions #'my/set-font-for-new-frame)

;; 通常起動時（非daemon）や最初のフレームでも適用
(when (display-graphic-p)
  (my/set-default-font))

;; SKK
(use-package ddskk
  :ensure t
  :init
  ;; 辞書設定
  (setq skk-large-jisyo (expand-file-name "~/SKK-JISYO.L"))
  (setq skk-user-jisyo "~/.emacs.d/skk-jisyo")
  (setq skk-search-prog-list
	'((skk-search-jisyo-file skk-user-jisyo 0)
          (skk-search-jisyo-file skk-large-jisyo 0)))

  (require 'skk-inline)
  (setq skk-show-inline t)
  (setq skk-show-tooltip nil) ;; tooltip 表示は無効に

  (add-hook 'skk-mode-hook
            (lambda ()
              (when (fboundp 'skk-inline-start)
		(skk-inline-start))))

  ;; インジケータカラー
  (setq skk-indicator-use-cursor-color t)
  (setq skk-use-color-cursor t))

(global-set-key (kbd "C-x j") 'skk-mode)


;; Emacs 起動時にウィンドウを最大化
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(server-start)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(auctex brief clang-format company-box consult corfu ddskk deadgrep
	    dumb-jump go-mode iedit json-mode lsp-ui magit marginalia
	    multiple-cursors orderless org-roam rust-mode smartparens
	    typescript-mode vertico vterm xclip yaml-mode)))
