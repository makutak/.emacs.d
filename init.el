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

;; Emacs 本体の基本設定
(use-package emacs
  :ensure nil
  :custom
  (inhibit-startup-screen t)
  (ring-bell-function #'ignore)
  (make-backup-files nil)
  (auto-save-default nil)
  (large-file-warning-threshold 100000000)
  (use-short-answers t)
  :bind
  (("C-h" . delete-backward-char)
   ("RET" . newline-and-indent)
   ("C-t" . other-window)
   ("C-x C-b" . ibuffer)
   ("C-x =" . balance-windows))
  :init
  (setq-default indent-tabs-mode nil)
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
          mac-option-modifier 'none)))

(defun my/setup-makefile ()
  "Makefile では TAB が構文要素なので whitespace の TAB 強調を切る。
インデントの TAB 自体は `makefile-mode' が `indent-tabs-mode' を t にするので
ここで設定する必要はない。"
  (setq-local whitespace-style (remq 'tabs whitespace-style)))

(use-package make-mode
  :ensure nil
  :hook (makefile-mode . my/setup-makefile))

;; PATH（mise の shims / 本体）を Emacs のグローバル exec-path / PATH へ通す。
;; GUI Emacs は GNOME/Wayland セッションから起動され、その PATH に mise の shims が
;; 無い。session 層(environment.d / ~/.profile)での注入は Wayland では経路が不安定で
;; reboot 必須のため断念し、Emacs 側で確実に通す（タイマー等の非バッファ文脈でも
;; rg が解決できる）。
;; - mise shims  : rg 等の mise 管理コマンド本体（呼び出し時に cwd でバージョン解決）
;; - ~/.local/bin: mise 本体（global-mise-mode が実行する）
(dolist (dir (list (expand-file-name "~/.local/share/mise/shims")
                   (expand-file-name "~/.local/bin")))
  (when (file-directory-p dir)
    (add-to-list 'exec-path dir)
    (setenv "PATH" (concat dir path-separator (getenv "PATH")))))

;; mise.el は各バッファでプロジェクト別バージョンの「解決済み実パス」へ
;; exec-path を上書きする（base は上記 shims 入りのグローバル default）。
(use-package mise
  :demand t
  :config
  (global-mise-mode 1))

(use-package exec-path-from-shell
  :demand t
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize))

;; slime
(use-package slime
  :if (file-exists-p "~/.roswell/helper.el")
  :init (load "~/.roswell/helper.el"))


;; `go-mode` の設定
(use-package go-mode
  :demand t
  :mode "\\.go\\'"
  :custom
  (gofmt-command "goimports")
  :hook
  (go-mode . (lambda ()
               (setq-local tab-width 2)
               (setq-local indent-tabs-mode t)
               (add-hook 'before-save-hook #'gofmt-before-save nil t))))

;; Python: LSP は Pyright、format/lint は Ruff に分離する。
(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :hook ((python-mode python-ts-mode) . eglot-ensure)
  :config
  ;; Eglot の自動検出候補に関係なく、Python は Pyright のみに接続する。
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 . ("pyright-langserver" "--stdio"))))

(use-package apheleia
  :hook ((python-mode python-ts-mode) . apheleia-mode)
  :config
  ;; Apheleia 組み込みの `ruff' は `ruff format' を標準入力に対して実行する。
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff
        (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff))

(defun my/python-flymake-ruff-load ()
  "Python の Eglot 管理バッファに Ruff の Flymake backend を追加する。"
  (when (memq major-mode '(python-mode python-ts-mode))
    (flymake-ruff-load)))

(use-package flymake-ruff
  :commands flymake-ruff-load
  :hook (eglot-managed-mode . my/python-flymake-ruff-load))

;; `lsp-mode` の設定
(use-package lsp-mode
  :commands lsp
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (rust-mode . lsp-deferred))
  :custom
  ;; LSP の CAPF は有効のまま、company の自動起動を避けて Corfu に表示させる。
  (lsp-completion-provider :none)
  (lsp-enable-snippet nil)
  (lsp-go-gopls-server-args '("-remote=auto"))
  (lsp-session-folders-remove '("/usr/local/go/src" "~/go/pkg/mod"))
  (lsp-enable-file-watchers nil)
  (lsp-disabled-clients '(semgrep-ls))
  (lsp-enable-on-type-formatting nil)
  :custom-face
  (lsp-face-highlight-textual ((t (:background unspecified :underline t))))
  (lsp-face-highlight-read    ((t (:background unspecified :underline t))))
  (lsp-face-highlight-write   ((t (:background unspecified :underline t :weight bold)))))

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
              ("TAB" . corfu-insert)
              ("<tab>" . corfu-insert)
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous)
              ("<down>" . corfu-next)
              ("<up>" . corfu-previous))
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  (corfu-preselect 'first)
  (corfu-quit-no-match t)
  :init
  (global-corfu-mode))

;; ターミナル用のcorfuバックエンド
(use-package corfu-terminal
  :unless (display-graphic-p)
  :config
  (corfu-terminal-mode +1))

;; `magit` (Git クライアント)
(use-package magit
  :bind ("C-x g" . magit-status))

;; `vertico` (ミニバッファ補完)
(use-package vertico
  :init
  (vertico-mode))

;; `marginalia`（補足情報を表示）
(use-package marginalia
  :init
  (marginalia-mode))

;; `orderless`（部分一致検索）
(use-package orderless
  :custom
  (completion-styles '(orderless))  ;; `orderless` を Emacs の補完スタイルに設定
  (completion-category-overrides '((file (styles basic)))))  ;; `find-file` では通常の補完を使う

(defun my/consult-line-or-region ()
  "リージョン選択中はその文字列を初期クエリとして `consult-line` を起動する。"
  (interactive)
  (if (use-region-p)
      (let ((query (buffer-substring-no-properties (region-beginning) (region-end))))
        (deactivate-mark)
        (consult-line query))
    (consult-line)))

(defun my/consult-ripgrep-or-region ()
  "リージョン選択中はその文字列を初期クエリとして `consult-ripgrep` を起動する。"
  (interactive)
  (if (use-region-p)
      (let ((query (buffer-substring-no-properties (region-beginning) (region-end))))
        (deactivate-mark)
        (consult-ripgrep nil query))
    (consult-ripgrep)))

;; `consult`（高度な検索＆ナビゲーション）
(use-package consult
  :bind
  (("C-s" . my/consult-line-or-region)  ;; リージョン選択時はその文字列で検索
   ("M-y" . consult-yank-pop)   ;; `M-y`（履歴ペースト）を強化
   ("C-x b" . consult-buffer)   ;; `C-x b`（バッファ切り替え）を強化
   ("C-x C-r" . consult-recent-file)  ;; `C-x C-r` で最近開いたファイル一覧
   ("C-c r" . my/consult-ripgrep-or-region)  ;; プロジェクト全体を ripgrep で検索
   ("C-c G" . consult-git-grep)  ;; git 管理ファイルを grep で検索
   ("C-c f" . consult-fd)))     ;; ファイル名ファジー検索（VSCode Cmd+P 相当）

;; `embark`（候補へのコンテキストアクション）
(use-package embark
  :bind (("C-." . embark-act)
         ("C-;" . embark-dwim)))

;; `embark-consult`（embark と consult の統合）
(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; `wgrep`（grep/ripgrep 結果バッファを直接編集）
(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))


;; `flymake` (静的解析)
(use-package flymake
  :hook ((go-mode . flymake-mode)
         (rust-mode . flymake-mode)
         (c-mode . (lambda ()
                     (remove-hook 'flymake-diagnostic-functions #'flymake-cc t)))))

;; 行番号表示
(use-package display-line-numbers
  :ensure nil
  :init
  (global-display-line-numbers-mode t))

(defun my/org-disable-electric-angle-pair ()
  "Org バッファでは `<` の自動ペア挿入を無効にする。"
  (setq-local electric-pair-inhibit-predicate
              (lambda (char) (char-equal char ?<))))

(use-package elec-pair
  :ensure nil
  :hook (org-mode . my/org-disable-electric-angle-pair)
  :init
  (electric-pair-mode t))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-t" . other-window)))

(defun my/clang-format-config ()
  "現在のバッファに適用される clang-format の設定を alist で返す。
`clang-format --dump-config' が親ディレクトリを遡って .clang-format を解決するので、
プロジェクトごとの設定も ~/.clang-format も同じ仕組みで拾える。
clang-format が無い・失敗した場合は nil。"
  (let* ((file (or buffer-file-name (expand-file-name "a.c")))
         (default-directory (file-name-directory file)))
    (when (executable-find "clang-format")
      (with-temp-buffer
        (when (eq 0 (call-process "clang-format" nil t nil
                                  "--style=file" "--dump-config"
                                  (concat "-assume-filename=" file)))
          (goto-char (point-min))
          (let (config)
            ;; 行頭のキーだけ拾う (字下げされた入れ子キーは無視する)
            (while (re-search-forward "^\\([A-Za-z]+\\):[ \t]+\\(.+?\\)[ \t]*$" nil t)
              (push (cons (match-string 1) (match-string 2)) config))
            config))))))

(defun my/cc-style-for-braces (brace)
  "clang-format の BreakBeforeBraces に対応する CC Mode のスタイル名を返す。
効くのは substatement-open (独立行のブレースを字下げするか) の違い。
GNU は字下げする、Whitesmiths は本文と同じ桁、それ以外は字下げしない。"
  (cond ((equal brace "GNU") "gnu")
        ((equal brace "Whitesmiths") "whitesmith")
        (t "bsd")))

(defun my/setup-c-mode ()
  "編集中のインデント (CC Mode) を、そのバッファに効く .clang-format に合わせる。
保存時の clang-format と編集中の CC Mode は別エンジンなので、
.clang-format を唯一の情報源として両方に同じ規則を教える。"
  (let ((config (my/clang-format-config)))
    (when config
      (let ((width  (string-to-number (or (cdr (assoc "IndentWidth" config)) "0")))
            (tabw   (string-to-number (or (cdr (assoc "TabWidth" config)) "0")))
            (usetab (or (cdr (assoc "UseTab" config)) "Never"))
            (brace  (or (cdr (assoc "BreakBeforeBraces" config)) "Attach")))
        ;; c-set-style がスタイル変数を上書きするので、必ず先に呼ぶ
        (c-set-style (my/cc-style-for-braces brace))
        (when (> width 0) (setq-local c-basic-offset width))
        (when (> tabw 0) (setq-local tab-width tabw))
        (setq-local indent-tabs-mode (not (equal usetab "Never")))))))

(use-package cc-mode
  :ensure nil
  :hook ((c-mode c++-mode) . my/setup-c-mode))

(defun my-clang-format-before-save ()
  "C/C++ の保存時に clang-format を自動実行"
  (condition-case err
      (clang-format-buffer)
    (error (message "clang-format error: %s" err))))

(defun my/enable-clang-format-on-save ()
  "C/C++ バッファで保存時の clang-format を有効にする。"
  (add-hook 'before-save-hook #'my-clang-format-before-save nil t))

(use-package clang-format
  :hook ((c-mode c++-mode) . my/enable-clang-format-on-save)
  :custom
  (clang-format-style "file"))  ;; .clang-format を参照

;; which-key
(use-package which-key
  :ensure nil
  :demand t
  :config
  (which-key-mode)
  (which-key-setup-side-window-right))

;; autorevert
(use-package autorevert
  :ensure nil
  :demand t
  :config
  (global-auto-revert-mode 1)
  (setq auto-revert-check-vc-info nil
        global-auto-revert-non-file-buffers t
        auto-revert-use-notify t
        auto-revert-avoid-polling t
        auto-revert-interval 5
        auto-revert-verbose nil)
  (with-eval-after-load 'tramp
    (setq vc-ignore-dir-regexp
          (format "\\(%s\\)\\|\\(%s\\)"
                  vc-ignore-dir-regexp
                  tramp-file-name-regexp))))

;; fcitx
(use-package fcitx
  :if (eq system-type 'gnu/linux)
  :config
  (setq fcitx-use-dbus (and (boundp 'dbus-registered-buses)
                            (executable-find "fcitx5-remote")
                            (= 0 (call-process "fcitx5-remote" nil nil nil nil))))
  (setq fcitx-remote-command "fcitx5-remote")
  (fcitx-aggressive-setup))

;; emacs-mozc: Emacs ネイティブのインライン日本語入力（mozc サーバと直接通信）。
;; XIM(fcitx) は lucid だとカーソル近くポップアップが天井で、バッファ内に下線付き
;; インラインを出せるのはこれだけ。Emacs 内では fcitx の Ctrl+Space ではなく
;; C-\(toggle-input-method, 既定キー) で切り替える。
;; mozc.el は Debian の emacs-mozc が site-lisp へ入れる版を使う（mozc_emacs_helper
;; とプロトコルが一致するので :ensure nil）。helper が無い環境(未導入/Mac)では無効。
(use-package mozc
  :ensure nil
  :if (and (eq system-type 'gnu/linux)
           (executable-find "mozc_emacs_helper"))
  :custom
  (default-input-method "japanese-mozc")
  ;; 変換候補をカーソル近くのオーバーレイに出す（echo-area より見やすい）
  (mozc-candidate-style 'overlay))


;; `rust-mode`
(use-package rust-mode
  :hook (rust-mode . (lambda () (add-hook 'before-save-hook 'rust-format-buffer nil 'local))))

;; `expand-region`
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; `multiple-cursors`
(use-package multiple-cursors
  :bind ("M-D" . mc/mark-next-like-this))

;; `iedit`
(use-package iedit
  :bind ("C-c i" . iedit-mode))

;; `whitespace` 設定
(defun my/enable-delete-trailing-whitespace-on-save ()
  "プログラム用バッファで末尾空白を保存時に削除する。"
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(use-package whitespace
  :custom
  (whitespace-style '(face empty tabs trailing))
  :hook (prog-mode . my/enable-delete-trailing-whitespace-on-save)
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


(defun my/lisp-auto-format ()
  "LISP 系のファイルを保存するときに自動フォーマットする。"
  (indent-region (point-min) (point-max)))

(defun my/enable-lisp-auto-format-on-save ()
  "Lisp 系バッファで保存時の自動フォーマットを有効にする。"
  (add-hook 'before-save-hook #'my/lisp-auto-format nil t))

(dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook lisp-interaction-mode-hook
                                     scheme-mode-hook common-lisp-mode-hook))
  (add-hook hook #'my/enable-lisp-auto-format-on-save))

(when (getenv "WAYLAND_DISPLAY")
  (use-package xclip
    :config
    (setq xclip-method 'wl-copy)
    (xclip-mode 1)))


;; dumb-jump を xref に統合（fallback的に）
(use-package dumb-jump
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-aggressive nil)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; xref を使った統一ジャンプキー
(use-package xref
  :ensure nil
  :bind (("M-." . xref-find-definitions)
         ("M-," . xref-go-back)))

;; TAGS 読み込み（etags）
;; TAGSファイルは .dir-locals.el 側で tags-table-list を指定する
;; init.el 側では、TAGS 機能の自動追加を無効にするだけでOK
(use-package etags
  :ensure nil
  :custom
  (tags-add-table nil)
  (tags-revert-without-query t))

;; cscope
(use-package xcscope
  :init
  (cscope-setup))


(defvar my/tags-update-processes (make-hash-table :test #'equal)
  "プロジェクトルートごとの実行中の TAGS 更新プロセス。")

(defun my/start-tags-and-cscope-update (project-root)
  "PROJECT-ROOT で TAGS / cscope.out の更新を開始する。"
  (let ((default-directory project-root))
    (let ((process (start-process "make-update" nil "make" "update")))
      (process-put process 'project-root project-root)
      (puthash project-root process my/tags-update-processes)
      (set-process-sentinel
       process
       (lambda (finished-process _event)
         (when (memq (process-status finished-process) '(exit signal))
           (let ((root (process-get finished-process 'project-root))
                 (rerun (process-get finished-process 'rerun)))
             (when (eq finished-process (gethash root my/tags-update-processes))
               (remhash root my/tags-update-processes)
               (when rerun
                 (my/start-tags-and-cscope-update root))))))))))

(defun my/update-tags-and-cscope ()
  "C/H ファイルの保存時に TAGS / cscope.out を再生成する。"
  (when (and buffer-file-name
             (string-match-p "\\.[ch]\\'" buffer-file-name))
    (let ((project-root (locate-dominating-file buffer-file-name "Makefile")))
      (when (and project-root (executable-find "make"))
        (let ((process (gethash project-root my/tags-update-processes)))
          (if (and process (process-live-p process))
              (process-put process 'rerun t)
            (my/start-tags-and-cscope-update project-root)))))))

(add-hook 'after-save-hook #'my/update-tags-and-cscope)

;; man
(use-package man
  :ensure nil
  :bind ("C-c m" . man))

(use-package org
  :pin gnu
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :custom
  (org-agenda-files '("~/org"))
  (org-default-notes-file "~/org/notes.org")
  (org-hide-leading-stars t)
  (org-startup-indented t)
  (org-pretty-entities t)
  (org-use-sub-superscripts '{})
  (org-export-with-sub-superscripts '{})
  (org-log-done 'time)
  (org-return-follows-link t)
  (org-capture-templates
   '(("i" "Inbox" entry (file "~/org/inbox.org")
      "* %?\n  %U" :empty-lines 1)
     ("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
      "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
     ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
      "* %?\n  %U\n  %a\n  %i" :empty-lines 1)))
  :config
  (require 'org-tempo))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit default :weight bold))))
 '(org-level-2 ((t (:inherit default :weight bold))))
 '(org-level-3 ((t (:inherit default :weight bold))))
 '(org-level-4 ((t (:inherit default :weight bold)))))

;; (use-package org-roam
;;   :ensure t
;;   :custom
;;   (org-roam-directory "~/org-roam")
;;   :bind (("C-c n l" . org-roam-buffer-toggle)
;;          ("C-c n f" . org-roam-node-find)
;;          ("C-c n g" . org-roam-graph))
;;   :config
;;   (org-roam-db-autosync-mode))

(defun my/save-scratch-to-inbox ()
  "Append the contents of *scratch* to ~/org/inbox.org as a new org entry."
  (interactive)
  (let ((scratch-buffer (get-buffer "*scratch*"))
        (inbox-path "~/org/inbox.org"))
    (when scratch-buffer
      (let ((content (with-current-buffer scratch-buffer
                       (buffer-substring-no-properties (point-min) (point-max)))))
        (unless (string-blank-p content)
          (with-current-buffer (find-file-noselect inbox-path)
            (goto-char (point-max))
            (insert (format "\n* Scratch Memo [%s]\n%s\n"
                            (format-time-string "%Y-%m-%d %H:%M")
                            content))
            (save-buffer)))
        (with-current-buffer scratch-buffer
          (erase-buffer)
          (insert ";; scratch buffer cleared\n"))))))
(global-set-key (kbd "C-c s") 'my/save-scratch-to-inbox)


(use-package asm-mode
  :mode ("\\.s\\'" . asm-mode)
  :hook (asm-mode . (lambda ()
                      (setq-local tab-width 2)
                      (setq-local indent-tabs-mode nil)
                      (setq-local asm-indent-level 2))))

(use-package lua-mode)

(use-package vterm
  :if (eq system-type 'gnu/linux)
  :custom-face
  (vterm-color-blue ((t (:foreground "#5F87AF" :background "#5F87AF"))))
  :config
  (define-key vterm-mode-map (kbd "C-h") #'vterm--self-insert))


(defun my/replace-commas-with-newlines (start end)
  "Replace all commas with newlines in the region from START to END.
If no region is active, apply to the entire buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (perform-replace "," "\n" nil nil nil nil nil start end))
(global-set-key (kbd "C-c ,") #'my/replace-commas-with-newlines)

;; フォント設定関数（システムに応じてフォントサイズを変える）
(defun my/set-default-font ()
  (let ((font-size (cond
                    ((eq system-type 'darwin) 16)
                    ((eq system-type 'gnu/linux) 13)
                    (t 14)))  ;; fallback
        (font-name "UDEV Gothic 35NF"))
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

;; JSON Format
(defun my/format-json-region ()
  "Use jq to pretty-print the selected JSON region."
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "jq ." t t))
;; C-c j にバインド
(global-set-key (kbd "C-c j") #'my/format-json-region)

(defun my/split-window-thirds ()
  (interactive)
  (let ((width (/ (frame-width) 3)))
    (split-window-right width)
    (other-window 1)
    (split-window-right width)
    (other-window -1)))

(global-set-key (kbd "C-x @") 'my/split-window-thirds)

(use-package winner
  :ensure nil
  :init
  (winner-mode 1))

(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))

;; Ghostty (Kitty keyboard protocol) で C-@ が \e[64;5u として届くのを修正
(define-key input-decode-map "\e[64;5u" (kbd "C-@"))

;; Freeze investigation: remove after capturing a backtrace.
(setq debug-on-quit t
      debug-on-event 'sigusr2
      garbage-collection-messages t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
