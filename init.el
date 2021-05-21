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
(setq exec-path
      (append
       (list
        (expand-file-name "~/.local/bin")
        (expand-file-name "~/.cargo/bin")
        (expand-file-name "~/.anyenv/envs/rbenv/shims/ruby"))
       exec-path))

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
         ("C-t" . other-window)
         ))

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
           ("C-M-f" . counsel-rg))
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


;; TODO counsel-gtags どうする？

;; (ivy-mode 1)
;; (counsel-mode 1)
;; (setq )
;; (setq )
;; (setq )
;; (setq
;;       )

;; (require 'ivy-xref)
;; (define-key global-map [remap find-file] #'counsel-find-file)
;; (define-key global-map [remap execute-extended-command] #'counsel-M-x)
;; (setq counsel-find-file-ignore-regexp (regexp-opt '("./" "../")))
;; ;;(define-key global-map [remap switch-to-buffer] #'helm-mini)

;; (global-set-key "\C-s" 'swiper)
;; (setq ) ;; 行数で検索を可能に

;; (global-set-key (kbd "M-x") 'counsel-M-x)
;; (global-set-key (kbd "M-y") 'counsel-yank-pop)
;; ;; key-bind 何とかする ここから
;; (global-set-key (kbd "C-M-z") 'counsel-fzf) ;; ファジー検索
;; (global-set-key (kbd "C-M-r") 'counsel-recentf)
;; (global-set-key (kbd "C-x C-b") 'counsel-ibuffer)
;; ;;(global-set-key (kbd "C-M-f") 'counsel-ag) ;; repository内で全文検索(ag)
;; (global-set-key (kbd "C-M-f") 'counsel-rg) ;; repository内で全文検索(ripgrep)
;; key-bind 何とかする ここまで

;;(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(imenu-list-position (quote left))
 '(imenu-list-size 30)
 '(package-archives
   (quote
    (("gnu" . "https://elpa.gnu.org/packages/")
     ("melpa" . "https://melpa.org/packages/")
     ("org" . "https://orgmode.org/elpa/"))))
 '(package-selected-packages
   (quote
    (ivy-prescient prescient color-theme-sanityinc-tomorrow macrostep leaf-tree leaf-convert blackout el-get leaf-keywords leaf lsp-mode yasnippet lsp-treemacs lsp-ivy counsel projectile hydra flycheck company avy which-key ivy-xref dap-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
