;;; Personal configuration -*- lexical-binding: t -*-

;; Save the contents of this file under ~/.emacs.d/init.el
;; Do not forget to use Emacs' built-in help system:
;; Use C-h C-h to get an overview of all help commands.  All you
;; need to know about Emacs (what commands exist, what functions do,
;; what variables specify), the help system can provide.

;; Add the NonGNU ELPA package archive
(require 'package)
(add-to-list 'package-archives  '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; (unless package-archive-contents
;;   (package-refresh-contents))

;; Load a custom theme
(load-theme 'tsdh-dark t)

(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-t") 'other-window)

;; Set default font face
(set-face-attribute 'default nil :font "Ricty-14")

;; Disable the menu bar
(menu-bar-mode -1)

;; Disable the tool bar
(tool-bar-mode -1)

;; Disable the scroll bars
(scroll-bar-mode -1)

;; Disable splash screen
(setq inhibit-startup-screen t)

;; Enable line numbering by default
(global-display-line-numbers-mode t)

;; Automatically pair parentheses
(electric-pair-mode t)

;; バックアップファイルを作成しない
(setq make-backup-files nil)
;; オートセーブファイルを作成しない
(setq auto-save-default nil)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; Vertico configuration
(unless (package-installed-p 'vertico)
  (package-install 'vertico))

;; Enable Vertico
(require 'vertico)
(vertico-mode)

;; Optionally enable cycling for `vertico-next` and `vertico-previous`.
(setq vertico-cycle t)

;; If you want to save and restore the last input of minibuffer commands
;; across Emacs sessions, uncomment the following lines:
;; (savehist-mode 1)
;; (add-to-list 'savehist-additional-variables 'vertico-directory-input-string)

;; golang
(unless (package-installed-p 'go-mode)
  (package-install 'go-mode))
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(setq gofmt-command "goimports")
(add-hook 'go-mode-hook (lambda ()
                          ;; タブ幅の設定
                          (setq tab-width 4)
                          ;; foramt
                          (add-hook 'before-save-hook 'gofmt-before-save)))


;; python
(unless (package-installed-p 'python-black)
  (package-install 'python-black))
(require 'python-black)

(defun my-python-mode-settings ()
  "python mode setting"
  (setq-local lsp-enable-formatting nil)
  (python-black-on-save-mode))
(add-hook 'python-mode-hook 'my-python-mode-settings)

;; C/C++
;;; ClangFormatの設定
(unless (package-installed-p 'clang-format)
  (package-install 'clang-format))

(require 'clang-format)
(setq clang-format-style "google")

;;; ファイル保存時にClangFormatを実行
(defun my-clang-format-before-save ()
  (interactive)
  (when (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
    (clang-format-buffer)))

(add-hook 'before-save-hook 'my-clang-format-before-save)

;; rust
;;; Rust support
(unless (package-installed-p 'rust-mode)
  (package-install 'rust-mode))
(require 'rust-mode)

;;; LSP Support
(unless (package-installed-p 'lsp-mode)
  (package-install 'lsp-mode))

(unless (package-installed-p 'lsp-pyright)
  (package-install 'lsp-pyright))

(setq lsp-completion-min-length 2)
(setq lsp-completion-provider :none)

(require 'lsp-mode)
(add-hook 'c-mode-hook #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)
(add-hook 'python-mode-hook #'lsp-deferred)
(add-hook 'go-mode-hook #'lsp-deferred)
(add-hook 'rust-mode-hook #'lsp-deferred)

(with-eval-after-load 'lsp-pyright
  (setq lsp-pyright-python-executable-cmd
        (executable-find "python3")))

(setq lsp-enable-snippet nil)
(setq lsp-enable-indentation t)
(setq lsp-keep-workspace-alive nil)
(setq lsp-signature-auto-activate nil)

;;; Inline static analysis
;; Enabled inline static analysis
;;(add-hook 'prog-mode-hook #'flymake-mode)
(add-hook 'c-mode-hook #'flymake-mode)
(add-hook 'c++-mode-hook #'flymake-mode)
(add-hook 'python-mode-hook #'flymake-mode)
(add-hook 'go-mode-hook #'flymake-mode)

;;; Pop-up completion
(unless (package-installed-p 'corfu)
  (package-install 'corfu))
;; Corfuの基本設定
(require 'corfu)
(global-corfu-mode)

;; 補完候補を循環する
(setq corfu-cycle t)

;; 自動補完を有効にする
(setq corfu-auto t)

;; 境界で自動的に補完を閉じる
(setq corfu-quit-at-boundary t)

;; 一致しない場合に補完を閉じる
(setq corfu-quit-no-match t)
(setq corfu-auto-delay 0.1)  ; 単位は秒

;; キーバインドの設定
(define-key corfu-map (kbd "TAB") #'corfu-next)
(define-key corfu-map (kbd "S-TAB") #'corfu-previous)


;;; Git client
(unless (package-installed-p 'magit)
  (package-install 'magit))

;; Bind the `magit-status' command to a convenient key.
(global-set-key (kbd "C-c g") #'magit-status)

;;; JSON Support
(unless (package-installed-p 'json-mode)
  (package-install 'json-mode))

;;; NASM Support
(unless (package-installed-p 'nasm-mode)
  (package-install 'nasm-mode))

;;; Typescript Support
(unless (package-installed-p 'typescript-mode)
  (package-install 'typescript-mode))

;;; YAML Support
(unless (package-installed-p 'yaml-mode)
  (package-install 'yaml-mode))

;;; LaTeX support
(unless (package-installed-p 'auctex)
  (package-install 'auctex))
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;;; Markdown support
(unless (package-installed-p 'markdown-mode)
  (package-install 'markdown-mode))

;;; Outline-based notes management and organizer

;;; Brief Emulation
(unless (package-installed-p 'brief)
  (package-install 'brief))

;; Miscellaneous options
(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))
(recentf-mode t)
(defalias 'yes-or-no #'y-or-n-p)

(unless (package-installed-p 'multiple-cursors)
  (package-install 'multiple-cursors))
(require 'multiple-cursors)
(global-set-key (kbd "M-D") 'mc/mark-next-like-this)

(unless (package-installed-p 'iedit)
  (package-install 'iedit))
(require 'iedit)

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Store automatic customisation options elsewhere
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; 大きなファイルの自動読み込みを防ぐ
(setq large-file-warning-threshold 100000000)  ; 100MB以上のファイルに対する警告

;; 特定のファイル拡張子を無視する
(add-to-list 'completion-ignored-extensions ".bin")
(add-to-list 'completion-ignored-extensions ".so")
(add-to-list 'completion-ignored-extensions ".o")

(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (dotimes (i (- num_wins 1))
    (split-window-horizontally))
  (balance-windows))

(global-set-key "\C-x@" (lambda ()
                           (interactive)
                           (split-window-horizontally-n 3)))
