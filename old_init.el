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
(setq exec-path (cons (expand-file-name "~/.local/bin") exec-path))
(setq exec-path (cons (expand-file-name "~/.cargo/bin") exec-path))
(setq exec-path (cons (expand-file-name "~/.anyenv/envs/rbenv/shims/ruby") exec-path))


(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (dotimes (i (- num_wins 1))
    (split-window-horizontally))
  (balance-windows))

;;画面を三分割
(global-set-key "\C-x@" (lambda ()
                          (interactive)
                          (split-window-horizontally-n 3)))

(setenv "GTAGSLIBPATH" (concat
                        "~/edk2/MdePkg/Include"
                        ":"
                        "~/edk2/MdePkg/Include/X64"))

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

;; theme
(leaf color-theme-sanityinc-tomorrow
  :doc "A version of Chris Kempson's \"tomorrow\" themes"
  :tag "themes" "faces"
  :added "2021-01-11"
  :url "https://github.com/purcell/color-theme-sanityinc-tomorrow"
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-eighties t))


;; ivy
(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :blackout t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . nil)
           (ivy-use-selectable-prompt . t))
  :global-minor-mode t
  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind (("C-s" . swiper)))

  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :blackout t
    :bind (("C-S-s" . counsel-imenu)
           ("C-x C-r" . counsel-recentf))
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t)

  (leaf counsel-gtags
    :doc "ivy for GNU global"
    :req "emacs-25.1" "counsel-0.8.0" "seq-1.0"
    :tag "emacs>=25.1"
    :added "2021-05-18"
    :url "https://github.com/FelipeLema/emacs-counsel-gtags"
    :emacs>= 25.1
    :ensure t
    :after counsel
    :bind ("M-."     . counsel-gtags-dwim)))

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


;; flycheck
;; (leaf flycheck
;;   :doc "On-the-fly syntax checking"
;;   :req "dash-2.12.1" "pkg-info-0.4" "let-alist-1.0.4" "seq-1.11" "emacs-24.3"
;;   :tag "minor-mode" "tools" "languages" "convenience" "emacs>=24.3"
;;   :url "http://www.flycheck.org"
;;   :emacs>= 24.3
;;   :ensure t
;;   :bind (("M-n" . flycheck-next-error)
;;          ("M-p" . flycheck-previous-error))
;;   :global-minor-mode global-flycheck-mode)


;; company
(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
  ;;:after c++-mode c-mode
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
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
  :global-minor-mode global-company-mode
  :config
  (eval-after-load 'company
    '(progn
       (setq company-backends (delete 'company-semantic company-backends)))))

(leaf company-c-headers
  :doc "Company mode backend for C/C++ header files"
  :req "emacs-24.1" "company-0.8"
  :tag "company" "development" "emacs>=24.1"
  :added "2021-05-18"
  :emacs>= 24.1
  :ensure t
  :after company
  :defvar company-backends
  :init
  (add-to-list 'company-backends 'company-c-headers)
  :config
  (eval-after-load 'company-c-headers
    '(progn
       (add-to-list 'company-c-headers-path-system "~/edk2/MdePkg/Include")
       (add-to-list 'company-c-headers-path-system "~/edk2/MdePkg/Include/X64"))))

;; paredit
(leaf paredit
  :doc "minor mode for editing parentheses"
  :tag "lisp"
  :added "2021-01-11"
  :ensure t
  :hook ((emacs-lisp-mode-hook . enable-paredit-mode)
         (lisp-mode-hook . enable-paredit-mode)
         (lisp-interacton-mode-hook . enable-paredit-mode)
         (scheme-mode-hook . enable-paredit-mode)))

;; rainbow-delimiters
(leaf rainbow-delimiters
  :doc "Highlight brackets according to their depth"
  :tag "tools" "lisp" "convenience" "faces"
  :added "2021-01-11"
  :url "https://github.com/Fanael/rainbow-delimiters"
  :ensure t
  :hook (prog-mode-hook))

(leaf smart-jump
  :doc "Smart go to definition."
  :req "emacs-25.1"
  :tag "tools" "emacs>=25.1"
  :added "2021-01-11"
  :url "https://github.com/jojojames/smart-jump"
  :emacs>= 25.1
  :ensure t
  :config
  (smart-jump-setup-default-registers))


(leaf eldoc
  :doc "Show function arglist or variable docstring in echo area"
  :tag "builtin"
  :added "2021-01-11"
  :hook ((emacs-lisp-mode-hook . turn-on-eldoc-mode)))

(leaf xclip
  :doc "Copy&paste GUI clipboard from text terminal"
  :tag "tools" "convenience"
  :added "2021-01-11"
  :url "http://elpa.gnu.org/packages/xclip.html"
  :ensure t
  :config
  (setq x-select-enable-clipboard t))

(leaf yasnippet
  :doc "Yet another snippet extension for Emacs"
  :req "cl-lib-0.5"
  :tag "emulation" "convenience"
  :added "2021-01-11"
  :url "http://github.com/joaotavora/yasnippet"
  :ensure t)

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 1))
  :global-minor-mode global-auto-revert-mode)

;; lsp
(leaf lsp-mode
  :doc "LSP mode"
  :req "emacs-26.1" "dash-2.14.1" "dash-functional-2.14.1" "f-0.20.0" "ht-2.0" "spinner-1.7.3" "markdown-mode-2.3" "lv-0"
  :tag "languages" "emacs>=26.1"
  :added "2021-01-11"
  :url "https://github.com/emacs-lsp/lsp-mode"
  :emacs>= 26.1
  :ensure t
  ;; :after spinner markdown-mode lv
  :init (yas-global-mode)
  :bind ("C-c h" . lsp-describe-thing-at-point)
  :custom (lsp-prefer-capf . t)
  :config
  (setq lsp-rust-analyzer-cargo-load-out-dirs-from-check t)
  (setq lsp-rust-analyzer-proc-macro-enable t)
  (leaf lsp-ui
    :doc "UI modules for lsp-mode"
    :req "emacs-26.1" "dash-2.14" "dash-functional-1.2.0" "lsp-mode-6.0" "markdown-mode-2.3"
    :tag "tools" "languages" "emacs>=26.1"
    :added "2021-01-11"
    :url "https://github.com/emacs-lsp/lsp-ui"
    :emacs>= 26.1
    :ensure t
    :custom
    ((lsp-ui-flycheck-enable . t)
     (lsp-prefer-flymake . nil))))


;; rust
(leaf rust-mode
  :doc "A major emacs mode for editing Rust source code"
  :req "emacs-25.1"
  :tag "languages" "emacs>=25.1"
  :added "2021-01-11"
  :url "https://github.com/rust-lang/rust-mode"
  :emacs>= 25.1
  :ensure t
  :custom ((rust-format-on-save . t))
  :bind ("M-RET" . lsp-execute-code-action)
  :hook (rust-mode-hook . lsp)
  :config
  (leaf cargo
    :doc "Emacs Minor Mode for Cargo, Rust's Package Manager."
    :req "emacs-24.3" "rust-mode-0.2.0" "markdown-mode-2.4"
    :tag "tools" "emacs>=24.3"
    :added "2021-01-11"
    :emacs>= 24.3
    :ensure t
    :hook ((rust-mode-hook . cargo-minor-mode))))

(leaf flycheck-rust
  :doc "Flycheck: Rust additions and Cargo support"
  :req "emacs-24.1" "flycheck-28" "dash-2.13.0" "seq-2.3" "let-alist-1.0.4"
  :tag "convenience" "tools" "emacs>=24.1"
  :added "2021-01-11"
  :url "https://github.com/flycheck/flycheck-rust"
  :emacs>= 24.1
  :ensure t
  :after flycheck rust-mode)


;; ruby
(leaf ruby-mode
  :doc "Major mode for editing Ruby files"
  :tag "builtin"
  :added "2021-01-13"
  :custom
  (ruby-insert-encoding-magic-comment . nil)
  :config
  (leaf inf-ruby
    :doc "Run a Ruby process in a buffer"
    :req "emacs-24.3"
    :tag "ruby" "languages" "emacs>=24.3"
    :added "2021-01-13"
    :url "http://github.com/nonsequitur/inf-ruby"
    :emacs>= 24.3
    :ensure t)
  (leaf ruby-end
    :doc "Automatic insertion of end blocks for Ruby"
    :tag "ruby" "convenience" "speed"
    :added "2021-01-13"
    :url "http://github.com/rejeep/ruby-end"
    :ensure t)
  (leaf robe
    :doc "Code navigation, documentation lookup and completion for Ruby"
    :req "inf-ruby-2.5.1" "emacs-24.4"
    :tag "rails" "convenience" "ruby" "emacs>=24.4"
    :added "2021-01-13"
    :url "https://github.com/dgutov/robe"
    :emacs>= 24.4
    :ensure t
    :after inf-rub
    :hook ((ruby-mode . robe-mode))))

(leaf rbenv
  :doc "Emacs integration for rbenv"
  :tag "rbenv" "ruby"
  :added "2021-01-13"
  :url "https://github.com/senny/rbenv.el"
  :ensure t
  :config
  (setq rbenv-installation-dir "~/.anyenv/envs/rbenv"))


(leaf ggtags
  :doc "emacs frontend to GNU Global source code tagging system"
  :req "emacs-25"
  :tag "convenience" "tools" "emacs>=25"
  :added "2021-05-21"
  :url "https://github.com/leoliu/ggtags"
  :emacs>= 25
  :ensure t
  :init
  (setq ggtags-global-search-libpath-for-reference t))

;; (leaf cc-mode
;;   :doc "major mode for editing C and similar languages"
;;   :tag "builtin"
;;   :added "2021-02-06"
;;   :bind (c-mode-base-map
;;          ("C-c c" . compile))
;;   :hook ((c-mode-hook . lsp))
;;   :config
;;   (eval-after-load 'cc-mode
;;       '(progn
;;          (define-key c-mode-map  [(tab)] 'company-complete)
;;          (define-key c++-mode-map  [(tab)] 'company-complete))))

(require 'eglot)
(add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd-9"))
(add-hook 'c-mode-hook 'eglot-ensure)
(add-hook 'c++-mode-hook 'eglot-ensure)
(delete 'company-semantic company-backends)
(require 'cc-mode)
(define-key c-mode-map  [(tab)] 'company-complete)


;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (derived-mode-p 'c-mode 'c++-mode 'asm-mode)
;;               (ggtags-mode 1))))



;; (require 'cc-mode)
;; (require 'semantic)

;; ;; (global-semanticdb-minor-mode 1)
;; ;; (global-semantic-idle-scheduler-mode 1)

;; (semantic-mode 1)

;; (defun my-semantic-hook ()
;;   (semantic-add-system-include "~/edk2/MdePkg/Include" 'c-mode-hook)
;;   (semantic-add-system-include "~/edk2/MdePkg/Include/X64" 'c-mode-hook))

;; (add-hook 'semantic-init-hook 'my-semantic-hook)


(leaf google-c-style
  :doc "Google's C/C++ style for c-mode"
  :tag "tools" "c"
  :added "2021-02-19"
  :ensure t
  :hook
  (c-mode-common-hook . google-set-c-style))

(leaf clang-format
  :doc "Format code using clang-format"
  :req "cl-lib-0.3"
  :tag "c" "tools"
  :added "2021-02-19"
  :ensure t
  :config
  (setq clang-format-style-option "google")
  (add-hook 'c-mode-common-hook
            (function (lambda ()
                        (add-hook 'before-save-hook
                                  'clang-format-buffer)))))

;; ede begin
(global-ede-mode 1)
;; ede end
(add-hook 'c-mode-hook 'counsel-gtags-mode)


(leaf python-mode
  :doc "Python major mode"
  :tag "oop" "python" "processes" "languages"
  :added "2021-03-22"
  :url "https://gitlab.com/groups/python-mode-devs"
  :ensure t
  :config
  (setq python-indent-offset 4))

(leaf py-yapf
  :doc "Use yapf to beautify a Python buffer"
  :added "2021-03-22"
  :url "https://github.com/paetzke/py-yapf.el"
  :ensure t
  :hook
  (python-mode-hook . py-yapf-enable-on-save))

;; (leaf elpy
;;   :doc "Emacs Python Development Environment"
;;   :req "company-0.9.2" "emacs-24.4" "highlight-indentation-0.5.0" "pyvenv-1.3" "yasnippet-0.8.0" "s-1.11.0"
;;   :tag "tools" "languages" "ide" "python" "emacs>=24.4"
;;   :added "2021-03-22"
;;   :url "https://github.com/jorgenschaefer/elpy"
;;   :emacs>= 24.4
;;   :ensure t
;;   :after company highlight-indentation pyvenv yasnippet
;;   :init
;;   (elpy-enable))

(leaf lsp-pyright
  :doc "Python LSP client using Pyright"
  :req "emacs-26.1" "lsp-mode-7.0" "dash-2.18.0" "ht-2.0"
  :tag "lsp" "tools" "languages" "emacs>=26.1"
  :added "2021-03-22"
  :url "https://github.com/emacs-lsp/lsp-pyright"
  :emacs>= 26.1
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp))))



;; ここにいっぱい設定を書く


(provide 'init)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ede-project-directories (quote ("/home/tk/work/tmp/emacs")))
 '(imenu-list-position (quote left))
 '(imenu-list-size 30)
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/"))))
 '(package-selected-packages
   (quote
    (ggtags company-c-headers counsel-gtags lsp-pyright elpy py-yapf python-mode clang-format google-c-style robe inf-ruby rbenv ruby-end eglot lsp-ruby yasnippet xclip smart-jump flycheck-rust cargo rust-mode lsp-ui lsp-mode rainbow-delimiters paredit company flycheck ivy-prescient prescient counsel swiper ivy color-theme-sanityinc-tomorrow macrostep leaf-tree leaf-convert blackout el-get hydra leaf-keywords leaf))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
