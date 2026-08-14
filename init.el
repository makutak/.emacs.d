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

;; GC を idle 時にまとめて実行し、タイピング中の STW を抑える
(use-package gcmh
  :demand t
  :config
  (gcmh-mode 1))

;; Emacs 本体の基本設定
(use-package emacs
  :ensure nil
  :custom
  (inhibit-startup-screen t)
  (ring-bell-function #'ignore)
  ;; バックアップと自動保存。作業ディレクトリに `foo~' / `#foo#' を撒かないよう
  ;; 保存先を ~/.emacs.d 配下へ隔離した上で、両方とも有効にしておく
  ;; （強制終了時に未保存分を救えるのはこの2つだけ）。
  (make-backup-files t)
  (backup-directory-alist
   `(("." . ,(expand-file-name "backup/" user-emacs-directory))))
  (backup-by-copying t)               ; symlink / hard link を壊さない
  (version-control t)                 ; foo.~1~, foo.~2~ と世代を残す
  (kept-new-versions 10)
  (kept-old-versions 5)
  (delete-old-versions t)               ; 古い世代の削除を毎回聞かない
  (auto-save-default t)
  (auto-save-timeout 10)                ; 10 秒アイドルで
  (auto-save-interval 100)              ; または 100 打鍵ごとに
  (auto-save-file-name-transforms
   `((".*" ,(expand-file-name "auto-save/" user-emacs-directory) t)))
  (large-file-warning-threshold 100000000)
  (use-short-answers t)
  ;; リージョンが有効な間、Emacs はコマンド 1 回ごとにリージョン全体を文字列へ
  ;; コピーして X の PRIMARY 選択にセットする（command_loop_1）。長い範囲を
  ;; 選ぶとこれが打鍵ごとの O(範囲長) コピーになり、大量のゴミを生んで GC を
  ;; 誘発する（実測: 119K 文字の範囲で 1 打鍵あたり約 0.35MB）。
  ;; 代償は「Emacs で選択 → 他アプリに中クリック貼り付け」が効かなくなること。
  ;; 明示的なコピー (M-w) は CLIPBOARD 経由なので今まで通り動く。
  (select-active-regions nil)
  :bind
  (("C-h" . delete-backward-char)
   ("C-t" . other-window)
   ("RET" . newline-and-indent)
   ("C-x C-b" . ibuffer)
   ("C-x =" . balance-windows))
  :init
  (setq-default indent-tabs-mode nil)
  ;; backup 側は Emacs が必要時に自動生成するが、auto-save の転送先は
  ;; 作ってくれないので先に用意しておく（無いと保存のたびにエラーになる）。
  (make-directory (expand-file-name "auto-save/" user-emacs-directory) t)
  (when (eq system-type 'darwin)
    (setq mac-command-modifier 'meta
          mac-option-modifier 'none)))

;; クリック、ドラッグ、ホイールを入力段階で無効化する。
(use-package inhibit-mouse
  :demand t
  :custom
  (inhibit-mouse-adjust-mouse-highlight t)
  (inhibit-mouse-adjust-show-help-function t)
  :config
  (if (daemonp)
      (add-hook 'server-after-make-frame-hook #'inhibit-mouse-mode)
    (inhibit-mouse-mode 1)))

(use-package windmove
  :ensure nil
  :bind (("C-c <left>"  . windmove-left)
         ("C-c <right>" . windmove-right)
         ("C-c <up>"    . windmove-up)
         ("C-c <down>"  . windmove-down)))

(use-package delsel
  :ensure nil
  :config
  (delete-selection-mode t))

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

;; Python: LSP は lsp-mode + Pyright、format/lint は Ruff に分離する。
(use-package lsp-pyright
  :after lsp-mode
  :custom
  (lsp-pyright-langserver-command "pyright"))

(use-package apheleia
  :hook ((python-mode python-ts-mode) . apheleia-mode)
  :config
  ;; Apheleia 組み込みの `ruff' は `ruff format' を標準入力に対して実行する。
  (setf (alist-get 'python-mode apheleia-mode-alist) 'ruff
        (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff))

(defun my/python-flymake-ruff-load ()
  "Python の lsp-mode 管理バッファに Ruff の Flymake backend を追加する。"
  (when (memq major-mode '(python-mode python-ts-mode))
    (flymake-ruff-load)))

(use-package flymake-ruff
  :commands flymake-ruff-load
  :hook (lsp-managed-mode . my/python-flymake-ruff-load))

;; `lsp-mode` の設定
(use-package lsp-mode
  :commands lsp
  :hook ((c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         ((python-mode python-ts-mode) . lsp-deferred)
         (rust-mode . lsp-deferred))
  :custom
  ;; Super+l は OS の画面ロックに取られるため、LSP のプレフィックスを変更する。
  (lsp-keymap-prefix "C-c l")
  ;; LSP の CAPF は有効のまま、company の自動起動を避けて Corfu に表示させる。
  (lsp-completion-provider :none)
  (lsp-enable-snippet nil)
  (lsp-go-gopls-server-args '("-remote=auto"))
  (lsp-session-folders-remove '("/usr/local/go/src" "~/go/pkg/mod"))
  (lsp-enable-file-watchers nil)
  (lsp-disabled-clients '(semgrep-ls))
  (lsp-enable-on-type-formatting nil)
  (lsp-inlay-hint-enable t)
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

(defun my/c-lsp-capf-setup ()
  "C/C++ の LSP 補完に dabbrev を混ぜる。
`struct ifreq' の `ifr_name' / `ifr_flags' のように、実体がマクロで
構造体メンバーではないものは clangd の `.' 補完には原理的に出てこない
\(本当のメンバーは `ifr_ifrn' と `ifr_ifru' の2つだけ)。
そこで LSP の候補に、同じ major-mode のバッファから拾った単語を足す。
マクロ定義のあるヘッダを開いていれば `ifr.ifr_f' → `ifr_flags' が出る。
意味解析ではなく単なる文字列一致なので、あくまで補助。"
  (when (memq major-mode '(c-mode c++-mode))
    (setq-local completion-at-point-functions
                (list (cape-capf-super #'lsp-completion-at-point
                                       :with #'cape-dabbrev)))))

(use-package cape
  ;; `lsp-completion-mode' が completion-at-point-functions を書き換えた後に
  ;; 差し替えたいので、c-mode-hook ではなくこのフックに乗せる。
  :hook (lsp-completion-mode . my/c-lsp-capf-setup)
  :custom
  ;; 走査対象は同じ major-mode のバッファのみ (cape のデフォルト)。
  ;; 巨大な C バッファを大量に開くと打鍵ごとの走査が重くなるので、その時は
  ;; `current-buffer' に落とす。
  (cape-dabbrev-buffer-function #'cape-same-mode-buffers))

;; `magit` (Git クライアント)
(use-package magit
  :bind ("C-x g" . magit-status))

;; ミニバッファの多重起動を禁止
(setq enable-recursive-minibuffers nil)

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

;; `consult-lsp`（LSP のシンボル・診断を Consult で検索）
(use-package consult-lsp
  :after (consult lsp-mode)
  :config
  ;; lsp-mode のワークスペースシンボル検索を Consult UI に置き換える。
  (define-key lsp-mode-map [remap xref-find-apropos]
              #'consult-lsp-symbols))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

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

(use-package dmacro
  :bind ("C-S-e" . dmacro-exec))

;; `flymake` (静的解析)
(use-package flymake
  :hook ((go-mode . flymake-mode)
         (rust-mode . flymake-mode)
         (c-mode . (lambda ()
                     (remove-hook 'flymake-diagnostic-functions #'flymake-cc t)))))

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("C-t" . other-window)))

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

;; fcitx.el は「Emacs 内で外部 fcitx を使う」ための補助（プレフィックスキーや
;; ミニバッファで IM を自動 OFF にする）だった。バッファ内の日本語入力を mozc.el
;; へ移して Emacs 内で fcitx を使わなくなったため撤去する。
;; 副次的に、0.1 秒ごとのポーリングタイマーと、プレフィックスキーごとの
;; fcitx5-remote の fork+exec (実測 0.88ms/回) も無くなる。
;; なお fcitx-use-dbus の判定 (boundp 'dbus-registered-buses) は Emacs 30 では
;; 常に nil で、D-Bus 経路は最初から一度も使われていなかった。

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

(use-package paredit
  :hook ((emacs-lisp-mode . paredit-mode)
         (lisp-mode . paredit-mode)
         (lisp-interaction-mode . paredit-mode)
         (scheme-mode . paredit-mode)
         (common-lisp-mode . paredit-mode)))


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

;; man
(use-package man
  :ensure nil
  :bind ("C-c m" . man))

(defun my/org-prose-display ()
  "Org バッファを文章向けの見た目に整える。"
  ;; org-indent が階層構造をインデントで見せるので、行番号は桁を食うだけで
  ;; 情報量が薄い。プログラムのバッファでは今まで通り出る。
  (display-line-numbers-mode -1)
  ;; 行間を少し空ける（float は行高に対する比率）。
  ;; 副作用: org-modern-indent がブロック左端に描く │ (org-modern-indent.el:50-52)
  ;; は普通の文字なので、行間の分だけ縦線が破線に見える。気になるなら nil に。
  (setq-local line-spacing 0.1)
  ;; 長い行を折り返す。visual-line-mode が word-wrap を t にし、
  ;; word-wrap-by-category が和文の禁則処理を有効にする（空白の無い日本語でも
  ;; 文字カテゴリを見て折り返し位置を決める。無効だと窓幅で機械的に切れる）。
  (setq-local word-wrap-by-category t)
  (visual-line-mode 1))

(use-package org
  :pin gnu
  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :hook (org-mode . my/org-prose-display)
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
  (org-hide-emphasis-markers t)
  ;; 見出しのタグは org-modern が pill で描くので、空白を詰めて右端に寄せる
  ;; 必要がない。しかも右寄せ位置は string-width 基準なので、日本語の見出しでは
  ;; テーブルと全く同じ理屈で桁がずれる（valign は見出しには効かない）。
  ;; 0 にすると見出しの直後に置かれ、ずれようがなくなる。
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-agenda-tags-column 0)
  ;; 折りたたみの "..." を控えめな三点リーダに
  (org-ellipsis "…")
  ;; インライン画像が原寸で出てバッファを埋めるのを防ぐ（#+ATTR_ORG が優先）
  (org-image-actual-width '(600))
  (org-capture-templates
   '(("i" "Inbox" entry (file "~/org/inbox.org")
      "* %?\n  %U" :empty-lines 1)
     ("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
      "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)
     ("n" "Note" entry (file+headline "~/org/notes.org" "Notes")
      "* %?\n  %U\n  %a\n  %i" :empty-lines 1)))
  :config
  (require 'org-tempo)
  ;; Allow emphasis markers (~code~, *bold*, ...) to hug Japanese text.
  ;; org-emphasis-regexp-components is a defvar with a setter, so setq alone
  ;; would not recompute org-emph-re / org-verbatim-re.
  (org-set-emph-re 'org-emphasis-regexp-components
                   '("-[:space:]('\"{[:nonascii:]"
                     "-[:space:].,:!?;'\")}\\[[:nonascii:]"
                     "[:space:]" "." 1)))

(use-package org-modern
  :after org
  :custom
  ;;(org-modern-star 'replace)   ; 見出しの星を ◉○◈◇✳ に
  ;; テーブルの見た目は valign に任せる。org-modern-table は罫線行
  ;; (|---+---|) に overline と space の display property を載せるので、
  ;; 同じ行を書き換える valign と競合する。
  (org-modern-table nil)
  :init
  (global-org-modern-mode))

;; org-modern の block-fringe は org-indent-mode 下では自動的に無効化される。
;; org-startup-indented t なので、ブロック左端の縦線はこれで補う。
;; MELPA/GNU ELPA には無いパッケージなので :vc で GitHub から取得する
;; (:vc がある場合 use-package-always-ensure は無視される)。
(use-package org-modern-indent
  :vc (:url "https://github.com/jdtsmith/org-modern-indent" :rev :newest)
  :after org-modern
  :config
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

;; テーブルをピクセル単位で揃え直す。org は全角を2カラムとして桁揃えするが、
;; 実フォントの全角幅がぴったり2倍とは限らず、また org-hide-emphasis-markers で
;; 隠れた ~ は幅0で描画されるので、文字数ベースの桁揃えは表示上ずれる。
;; valign は window-text-pixel-size で「実際に描画された幅」を測るため両方に効く。
;; これも MELPA には無いので :vc で GitHub から取得する。
;; ソース中の文字ベースの桁揃えは書き換えないので、export やファイルの中身は不変。
(use-package valign
  :vc (:url "https://github.com/casouri/valign" :rev :newest)
  :after org
  ;; valign-mode は非 GUI では何もせずメッセージを出すだけ (valign.el:1149) なので、
  ;; 端末 Emacs で org を開くたびに鳴らないようフック側で弾く。
  :hook (org-mode . my/valign-mode-maybe)
  :init
  (defun my/valign-mode-maybe ()
    (when (display-graphic-p) (valign-mode 1))))

;; org-hide-emphasis-markers t の相棒。カーソルが要素に入った時だけマーカーを
;; 表示するので、普段は綺麗なまま `~foo~' の ~ を編集できる。
(use-package org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  ;; ただしテーブル内では抑制する。org-appear は表示する側では invisible
  ;; プロパティを直接外すだけで font-lock を再実行しない (org-appear.el:352-388)
  ;; ため、valign の桁詰めが古いまま残り、カーソルのある行だけ ~ の幅ぶん
  ;; 右にずれて見える（隠す側は font-lock-flush するので戻る）。
  ;; `org-appear--current-elem' は「対象外なら nil を返す」契約なので、
  ;; テーブル内で nil にすれば「要素の外にいる」通常状態として扱われる。
  (defun my/org-appear-skip-tables (elem)
    (unless (org-at-table-p) elem))
  (advice-add 'org-appear--current-elem :filter-return #'my/org-appear-skip-tables))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((((background dark)) (:inherit default :weight bold :foreground "#9ec1ff")) (t (:inherit default :weight bold :foreground "#12395f"))))
 '(org-level-2 ((((background dark)) (:inherit default :weight bold :foreground "#d4a8f0")) (t (:inherit default :weight bold :foreground "#5b2d78"))))
 '(org-level-3 ((((background dark)) (:inherit default :weight bold :foreground "#68d3ab")) (t (:inherit default :weight bold :foreground "#1a6b57"))))
 '(org-level-4 ((((background dark)) (:inherit default :weight bold :foreground "#e8b465")) (t (:inherit default :weight bold :foreground "#8a5a14"))))
 '(org-level-5 ((((background dark)) (:inherit default :weight bold :foreground "#8cc6e8")) (t (:inherit default :weight bold :foreground "#3d7396"))))
 '(org-level-6 ((((background dark)) (:inherit default :weight bold :foreground "#f29bb4")) (t (:inherit default :weight bold :foreground "#a3546a"))))
 '(org-level-7 ((((background dark)) (:inherit default :weight bold :foreground "#bdd68c")) (t (:inherit default :weight bold :foreground "#697a4a"))))
 '(org-level-8 ((((background dark)) (:inherit default :weight bold :foreground "#b4b4c6")) (t (:inherit default :weight bold :foreground "#75757f")))))

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
                    ((eq system-type 'darwin) 18)
                    ((eq system-type 'gnu/linux) 13)
                    (t 14)))  ;; fallback
        ;; "35" 系 (UDEV Gothic 35NF) は 半角:全角 = 3:5 = 1.667 なので、
        ;; 全角を2カラムとして桁揃えする org のテーブルが日本語行でずれる。
        ;; 無印の NF は 半角:全角 = 1:2 ちょうど (実測 'A'=1024 / '本'=2048)。
        (font-name "UDEV Gothic NF"))
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

(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))

;; Ghostty (Kitty keyboard protocol) で C-@ が \e[64;5u として届くのを修正
(define-key input-decode-map "\e[64;5u" (kbd "C-@"))

(defun my/update-all ()
  "Emacs パッケージと LSP サーバーをまとめて更新する。"
  (interactive)
  (when (yes-or-no-p "パッケージと LSP サーバーをすべて更新しますか？ ")
    ;; ELPA/MELPA と package-vc を更新する。
    (package-upgrade-all nil)

    ;; lsp-mode 管理下にある全サーバーの更新を開始する。
    (if (require 'lsp-mode nil t)
        (progn
          (lsp-update-servers)
          (message "パッケージ更新完了。LSP サーバー更新を非同期で開始しました。"))
      (message "パッケージ更新完了。lsp-mode は見つかりませんでした。"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-window apheleia auctex brief cape clang-format consult-dir consult-lsp
                corfu-terminal dmacro dumb-jump embark-consult
                exec-path-from-shell expand-region fcitx flymake-ruff
                gcmh go-mode iedit inhibit-mouse json-mode lsp-pyright lsp-ui
                lua-mode magit marginalia mise multiple-cursors
                orderless org-appear org-modern org-modern-indent
                paredit ruff-format rust-mode slime smartparens
                typescript-mode valign vertico wgrep xclip
                xcscope yaml-mode))
 '(package-vc-selected-packages
   '((org-modern-indent :url
                        "https://github.com/jdtsmith/org-modern-indent"))))
