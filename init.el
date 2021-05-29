;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; My init.el.

;;; Code:

;; Nihongo
(set-language-environment "Japanese")
(setq default-process-coding-system '(utf-8 . utf-8))
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
;; Use UTF-8.
(prefer-coding-system 'utf-8)
;; Prevent beeping.
(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(setq auto-save-default nil)
;;ツールバーをなくす
(tool-bar-mode -1)
;メニューバーをなくす
(menu-bar-mode -1)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq initial-major-mode 'emacs-lisp-mode)
;; Show key strokes in minibuffer quickly.
(setq echo-keystrokes 0.1)
;; スクロールは１行ごとに
(setq scroll-conservatively 1)
;;タブを2スペースに
(setq-default tab-width 2 indent-tabs-mode nil)
;; yes or no -> y or n
(fset 'yes-or-no-p 'y-or-n-p)
;;文末の空行を削除
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; Highlights matching parenthesis
(show-paren-mode 1)
;; Highlight current line
(global-hl-line-mode 1)
(electric-pair-mode 1)
;; diredのファイルサイズ単位を読みやすく
(setq dired-listing-switches (purecopy "-alh"))
;; exec path
(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
(add-to-list 'exec-path (expand-file-name "~/.anyenv/envs/rbenv/shims/ruby"))

;; 仮面分割用
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (dotimes (i (- num_wins 1))
    (split-window-horizontally))
  (balance-windows))

;;画面を三分割
(global-set-key "\C-x@" (lambda ()
                          (interactive)
                          (split-window-horizontally-n 3)))

;; エラー時にデバッガ起動
(setq debug-on-error t)


(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(leaf leaf
  :config
  (leaf leaf-convert :ensure t)
  (leaf leaf-tree
    :ensure t
    :custom ((imenu-list-size . 30)
             (imenu-list-position . 'left))))

(leaf macrostep
  :ensure t
  :bind (("C-c e" . macrostep-expand)))

;; keybindigs
(leaf key-binding
  :bind (("RET" . newline-and-indent)
         ("C-2" . set-mark-command)
         ("C-h" . backward-delete-char)
         ("C-z" . undo)
         ("C-t" . other-window)))

;; (leaf windmove-default-keybindings
;;   :config
;;   (windmove-default-keybindings))


(leaf leaf-convert
  :config
  (add-to-list 'default-frame-alist
               '(font . "ricty-13.5")))

;; color-theme
(leaf color-theme-sanityinc-tomorrow
  :doc "A version of Chris Kempson's \"tomorrow\" themes"
  :tag "themes" "faces"
  :added "2021-01-11"
  :url "https://github.com/purcell/color-theme-sanityinc-tomorrow"
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-eighties t))


;; 不要
;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; (package-initialize)

;; ここから必要なものを
;; (setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs lsp-ivy counsel
;;     projectile hydra flycheck company avy which-key ivy-xref dap-mode))

;; (when (cl-find-if-not #'package-installed-p package-selected-packages)
;;   (package-refresh-contents)
;;   (mapc #'package-install package-selected-packages))


(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :added "2021-05-25"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :blackout t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . nil)
           (ivy-use-selectable-prompt . t)
           (ivy-use-virtual-buffers . t)
           (enable-recursive-minibuffers . t)
           (ivy-extra-directories . nil)
           (ivy-re-builders-alist . '((t . ivy--regex-plus))))
  :global-minor-mode t
  :config
    (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind (("C-s" . swiper))
    :custom ((swiper-include-line-number-in-search . t)))

    (leaf counsel
      :doc "Various completion functions using Ivy"
      :req "emacs-24.5" "swiper-0.13.0"
      :tag "tools" "matching" "convenience" "emacs>=24.5"
      :url "https://github.com/abo-abo/swiper"
      :emacs>= 24.5
      :ensure t
      :blackout t
      :bind (("C-S-s" . counsel-imenu)
             ("C-x C-r" . counsel-recentf)
             ("C-x C-f" . counsel-find-file)
             ("M-x" . counsel-M-x)
             ("M-y" . counsel-yank-pop)
             ("C-M-z" . counsel-fzf)
             ("C-M-r" . counsel-recentf)
             ("C-x C-b" . counsel-ibuffer)
             ;("C-M-f" . counsel-rg)
             )
      :custom `((counsel-yank-pop-separator . "\n----------\n")
                (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
      :global-minor-mode t)

    (leaf ivy-xref
      :doc "Ivy interface for xref results"
      :req "emacs-25.1" "ivy-0.10.0"
      :tag "emacs>=25.1"
      :added "2021-05-25"
      :url "https://github.com/alexmurray/ivy-xref"
      :emacs>= 25.1
      :ensure t
      :after ivy))

(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :custom ((prescient-aggressive-file-save . t))
  :global-minor-mode prescient-persist-mode)

(leaf ivy-prescient
  :doc "prescient.el + Ivy"
  :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :after prescient ivy
  :custom ((ivy-prescient-retain-classic-highlighting . t))
  :global-minor-mode t)

(leaf which-key
  :doc "Display available keybindings in popup"
  :req "emacs-24.4"
  :tag "emacs>=24.4"
  :added "2021-05-25"
  :url "https://github.com/justbur/emacs-which-key"
  :emacs>= 24.4
  :ensure t)

(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :global-minor-mode delete-selection-mode)


;; flycheck
(leaf flycheck
  :doc "On-the-fly syntax checking"
  :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
  :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
  :url "http://www.flycheck.org"
  :emacs>= 24.3
  :ensure t
  :bind (("M-n" . flycheck-next-error)
         ("M-p" . flycheck-previous-error))
  :global-minor-mode global-flycheck-mode)

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-25.1"
  :tag "matching" "convenience" "abbrev" "emacs>=25.1"
  :added "2021-05-27"
  :url "http://company-mode.github.io/"
  :emacs>= 25.1
  :ensure t
  :blackout t
  :leaf-defer nil ;;遅延読み込みオフ
  :bind ((company-active-map
          ("M-n" . nil) ;; flycheckでkeybind設定しているので
          ("M-p" . nil) ;; flycheckでkeybind設定しているので
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode)

(leaf company-c-headers
  :doc "Company mode backend for C/C++ header files"
  :req "emacs-24.1" "company-0.8"
  :tag "company" "development" "emacs>=24.1"
  :added "2021-05-27"
  :emacs>= 24.1
  :ensure t
  :after company
  :defvar company-backends
  :config
  (add-to-list 'company-backends 'company-c-headers)
  (eval-after-load 'company-c-headers
    '(progn
       (add-to-list 'company-c-headers-path-system "~/edk2/MdePkg/Include")
       (add-to-list 'company-c-headers-path-system "~/edk2/MdePkg/Include/X64"))))

(leaf paredit
  :doc "minor mode for editing parentheses"
  :tag "lisp"
  :added "2021-05-27"
  :ensure t
  :hook ((emacs-lisp-mode-hook . enable-paredit-mode)
         (lisp-mode-hook . enable-paredit-mode)
         (lisp-interacton-mode-hook . enable-paredit-mode)
         (scheme-mode-hook . enable-paredit-mode)))

(leaf smart-jump
  :doc "Smart go to definition."
  :req "emacs-25.1"
  :tag "tools" "emacs>=25.1"
  :added "2021-05-27"
  :url "https://github.com/jojojames/smart-jump"
  :emacs>= 25.1
  :ensure t
  :config
  (smart-jump-setup-default-registers))

(leaf eldoc
  :doc "Show function arglist or variable docstring in echo area"
  :tag "builtin"
  :added "2021-05-27"
  :hook ((emacs-lisp-mode-hook . turn-on-eldoc-mode)))

(leaf xclip
  :doc "Copy&paste GUI clipboard from text terminal"
  :tag "tools" "convenience"
  :added "2021-05-27"
  :url "http://elpa.gnu.org/packages/xclip.html"
  :ensure t
  :custom ((x-select-enable-clipboard . t)))

(leaf yasnippet
  :doc "Yet another snippet extension for Emacs"
  :req "cl-lib-0.5"
  :tag "emulation" "convenience"
  :added "2021-05-27"
  :url "http://github.com/joaotavora/yasnippet"
  :ensure t)

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :added "2021-05-27"
  :custom ((autorevert-interval . 1))
  :global-minor-mode global-auto-revert-mode)

(leaf dap-mode
  :doc "Debug Adapter Protocol mode"
  :req "emacs-26.1" "dash-2.18.0" "lsp-mode-6.0" "bui-1.1.0" "f-0.20.0" "s-1.12.0" "lsp-treemacs-0.1" "posframe-0.7.0" "ht-2.3"
  :tag "debug" "languages" "emacs>=26.1"
  :added "2021-05-28"
  :url "https://github.com/emacs-lsp/dap-mode"
  :emacs>= 26.1
  :ensure t
  :after lsp-mode bui lsp-treemacs posframe)

(leaf flycheck-rust
  :doc "Flycheck: Rust additions and Cargo support"
  :req "emacs-24.1" "flycheck-28" "dash-2.13.0" "seq-2.3" "let-alist-1.0.4"
  :tag "convenience" "tools" "emacs>=24.1"
  :added "2021-01-11"
  :url "https://github.com/flycheck/flycheck-rust"
  :emacs>= 24.1
  :ensure t)

;; rust
(leaf rust-mode
  :doc "A major-mode for editing Rust source code"
  :req "emacs-25.1"
  :tag "languages" "emacs>=25.1"
  :added "2021-05-30"
  :url "https://github.com/rust-lang/rust-mode"
  :emacs>= 25.1
  :ensure t
  :custom ((rust-format-on-save . t))
  :bind ("M-RET" . lsp-execute-code-action)
  :hook ((rust-mode-hook . flycheck-rust-setup))
  :config
  (leaf cargo
    :doc "Emacs Minor Mode for Cargo, Rust's Package Manager."
    :req "emacs-24.3" "markdown-mode-2.4"
    :tag "tools" "emacs>=24.3"
    :added "2021-05-30"
    :emacs>= 24.3
    :ensure t
    :disabled t
    :after markdown-mode
    :hook ((rust-mode-hook . cargo-minor-mode))))

(leaf ruby-mode
  :doc "Major mode for editing Ruby files"
  :tag "builtin"
  :added "2021-05-31"
  :custom
  (ruby-insert-encoding-magic-comment . nil)
  :config
  (leaf inf-ruby
    :doc "Run a Ruby process in a buffer"
    :req "emacs-24.3"
    :tag "ruby" "languages" "emacs>=24.3"
    :added "2021-05-31"
    :url "http://github.com/nonsequitur/inf-ruby"
    :emacs>= 24.3
    :ensure t
    :hook ((ruby-mode-hook . inf-ruby-minor-mode))
    :config
    (autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t))
  (leaf ruby-end
    :doc "Automatic insertion of end blocks for Ruby"
    :tag "ruby" "convenience" "speed"
    :added "2021-05-31"
    :url "http://github.com/rejeep/ruby-end"
    :ensure t)
  (leaf robe
    :doc "Code navigation, documentation lookup and completion for Ruby"
    :req "inf-ruby-2.5.1" "emacs-24.4"
    :tag "rails" "convenience" "ruby" "emacs>=24.4"
    :added "2021-05-31"
    :url "https://github.com/dgutov/robe"
    :emacs>= 24.4
    :ensure t
    :after inf-ruby
    :hook ((ruby-mode-hook . robe-mode))))

(leaf rbenv
  :doc "Emacs integration for rbenv"
  :tag "rbenv" "ruby"
  :added "2021-05-31"
  :url "https://github.com/senny/rbenv.el"
  :ensure t
  :custom ((rbenv-installation-dir . "~/.anyenv/envs/rbenv")))

(leaf projectile
  :doc "Manage and navigate projects in Emacs easily"
  :req "emacs-25.1" "pkg-info-0.4"
  :tag "convenience" "project" "emacs>=25.1"
  :added "2021-05-31"
  :url "https://github.com/bbatsov/projectile"
  :emacs>= 25.1
  :ensure t
  :custom ((projectile-completion-system . 'ivy)
           (projectile-enable-caching . t))
  :bind ("C-c p" . projectile-command-map)
  :hook ((projectile-mode-hook . projectile-rails-global-mode))
  :global-minor-mode projectile-mode
  :config
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(leaf projectile-rails
  :doc "Minor mode for Rails projects based on projectile-mode"
  :req "emacs-24.3" "projectile-0.12.0" "inflections-1.1" "inf-ruby-2.2.6" "f-0.13.0" "rake-0.3.2"
  :tag "projectile" "rails" "emacs>=24.3"
  :added "2021-05-31"
  :url "https://github.com/asok/projectile-rails"
  :emacs>= 24.3
  :ensure t
  :after projectile inflections inf-ruby rake
  :bind ("C-c r" . projectile-rails-command-map)
  :global-minor-mode projectile-rails-global-mode)

(leaf quickrun
  :doc "Run commands quickly"
  :req "emacs-24.3"
  :tag "emacs>=24.3"
  :added "2021-06-01"
  :url "https://github.com/syohex/emacs-quickrun"
  :emacs>= 24.3
  :ensure t)

;; TODO: ruby lsp の設定


;; TODO counsel-gtags どうする？

;; (add-hook 'c-mode-hook 'lsp)
;; (add-hook 'c++-mode-hook 'lsp)

;; (setq gc-cons-threshold (* 100 1024 1024)
;;       read-process-output-max (* 1024 1024)
;;       treemacs-space-between-root-nodes nil
;;       company-idle-delay 0.0
;;       company-minimum-prefix-length 1
;;       lsp-idle-delay 0.1)  ;; clangd is fast

;; (with-eval-after-load 'lsp-mode
;;   (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
;;   (require 'dap-cpptools)
;;   (yas-global-mode))



;; lsp
(leaf lsp-mode
  :doc "LSP mode"
  :req "emacs-26.1" "dash-2.18.0" "f-0.20.0" "ht-2.3" "spinner-1.7.3" "markdown-mode-2.3" "lv-0"
  :tag "languages" "emacs>=26.1"
  :added "2021-05-28"
  :url "https://github.com/emacs-lsp/lsp-mode"
  :emacs>= 26.1
  :ensure t
  :init (yas-global-mode)
  :bind ("C-c h" . lsp-describe-thing-at-point)
  ;;:after spinner markdown-mode lv rust-mode
  :hook ((rust-mode-hook . lsp)
         (c-mode-hook . lsp))
  :custom ((lsp-rust-server . 'rust-analyzer)
           (lsp-rust-analyzer-cargo-load-out-dirs-from-check . t)
           (lsp-rust-analyzer-proc-macro-enable . t)
           (lsp-prefer-capf . t))
  :config
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))
  (leaf lsp-ui
    :doc "UI modules for lsp-mode"
    :req "emacs-26.1" "dash-2.18.0" "lsp-mode-6.0" "markdown-mode-2.3"
    :tag "tools" "languages" "emacs>=26.1"
    :added "2021-05-28"
    :url "https://github.com/emacs-lsp/lsp-ui"
    :emacs>= 26.1
    :ensure t
    :after lsp-mode markdown-mode
    :custom ((lsp-ui-flycheck-enable . t)
             (lsp-prefer-flymake . nil)))

  (leaf ccls
  :doc "ccls client for lsp-mode"
  :req "emacs-25.1" "lsp-mode-6.3.1" "dash-2.14.1"
  :tag "c++" "lsp" "languages" "emacs>=25.1"
  :added "2021-06-05"
  :url "https://github.com/MaskRay/emacs-ccls"
  :emacs>= 25.1
  :ensure t
  :after lsp-mode
  :custom  ((ccls-executable . "/usr/local/bin/ccls"))
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp)))))




(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(imenu-list-position (quote left) t)
 '(imenu-list-size 30 t)
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/"))))
 '(package-selected-packages
   (quote
    (ccls quickrun projectile-rails projectile rbenv robe ruby-end inf-ruby lsp-ui rust-mode flycheck-rust dap-mode yasnippet xclip smart-jump paredit company-c-headers company flycheck which-key ivy-prescient prescient ivy-xref counsel swiper ivy color-theme-sanityinc-tomorrow macrostep leaf-tree leaf-convert blackout el-get hydra leaf-keywords leaf))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
