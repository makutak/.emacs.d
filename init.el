;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; My init.el.

;;; Code:

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/init.el
(setq gc-cons-threshold (* 100 1024 1024))

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

;; ここにいっぱい設定を書く
(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :preface
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))

  :bind (("M-ESC ESC" . c/redraw-frame))
  :custom '((create-lockfiles . nil)
            (debug-on-error . t)
            (init-file-debug . t)
            (frame-resize-pixelwise . t)
            (enable-recursive-minibuffers . t)
            (history-length . 1000)
            (history-delete-duplicates . t)
            (scroll-preserve-screen-position . t)
            (scroll-conservatively . 1)
            (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
            (ring-bell-function . 'ignore)
            (text-quoting-style . 'straight)
            (truncate-lines . t)
            ;; (use-dialog-box . nil)
            ;; (use-file-dialog . nil)
            ;; (menu-bar-mode . t)
            ;; (tool-bar-mode . nil)
            (scroll-bar-mode . nil)
            (indent-tabs-mode . nil)
            (tab-width . 2)
            (make-backup-files . nil)
            (auto-save-default . nil)
            (tool-bar-mode . nil)
            ;;メニューバーをなくす
            (menu-bar-mode . nil)
            (max-specpdl-size . 10000)
            (max-lisp-eval-depth . 10000)
            (mac-command-modifier . 'meta)
            (show-trailing-whitespace . t)
            (indicate-empty-lines . t)
            (indicate-buffer-boundaries . 'left)
            (sentence-end-double-space . nil))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (keyboard-translate ?\C-h ?\C-?)
  ;;文末の空行を削除
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(leaf emacs-startup-setting
  :custom
  ((inhibit-startup-screen . t)
   (inhibit-startup-echo-area-message . t)
   (inhibit-startup-message . t)
   (initial-scratch-message . nil))
  :config
  (add-hook 'window-setup-hook 'toggle-frame-maximized t))

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 1))
  :global-minor-mode global-auto-revert-mode)

(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :global-minor-mode delete-selection-mode)

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :custom ((show-paren-delay . 0))
  :global-minor-mode show-paren-mode)

(leaf simple
  :doc "basic editing commands for Emacs"
  :tag "builtin" "internal"
  :custom ((kill-ring-max . 100)
           (kill-read-only-ok . t)
           (kill-whole-line . t)
           (eval-expression-print-length . nil)
           (eval-expression-print-level . nil)))

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
    :bind (("C-s" . swiper)
           ("C-x b" . ivy-switch-buffer)))

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
    :global-minor-mode t))

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
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
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
  :custom ((company-idle-delay . 0.0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode)

(leaf company-c-headers
  :doc "Company mode backend for C/C++ header files"
  :req "emacs-24.1" "company-0.8"
  :tag "company" "development" "emacs>=24.1"
  :added "2020-03-25"
  :emacs>= 24.1
  :ensure t
  :after company
  :defvar company-backends
  :config
  (add-to-list 'company-backends 'company-c-headers))

(leaf exec-path-from-shell
  :doc "Get environment variables such as $PATH from the shell"
  :req "emacs-24.1" "cl-lib-0.6"
  :tag "environment" "unix" "emacs>=24.1"
  :added "2021-06-08"
  :url "https://github.com/purcell/exec-path-from-shell"
  :emacs>= 24.1
  :ensure t
  :custom ((exec-path-from-shell-check-startup-files . nil)
           (exec-path-from-shell-variables . '("PATH"))))

(leaf paren
  :doc "highlight matching paren"
  :tag "builtin"
  :added "2021-06-08"
  :hook (emacs-startup-hook . show-paren-mode))

(leaf hl-line
  :doc "highlight the current line"
  :tag "builtin"
  :added "2021-06-08"
  :global-minor-mode t)

(leaf electric
  :doc "window maker and Command loop for `electric' modes"
  :tag "builtin"
  :added "2021-06-08"
  :init (electric-pair-mode 1))


;; keybindigs
(leaf key-binding
  :bind (("RET" . newline-and-indent)
         ("C-2" . set-mark-command)
         ("C-h" . backward-delete-char)
         ;;("C-z" . undo)
         ("C-t" . other-window)))

(leaf leaf-convert
  :config
  (add-to-list 'default-frame-alist
               '(font . "ricty-20")))

;; color-theme
(leaf color-theme-sanityinc-tomorrow
  :doc "A version of Chris Kempson's \"tomorrow\" themes"
  :tag "themes" "faces"
  :added "2021-01-11"
  :url "https://github.com/purcell/color-theme-sanityinc-tomorrow"
  :ensure t
  :config
  (load-theme 'sanityinc-tomorrow-eighties t))

(leaf paredit
  :doc "minor mode for editing parentheses"
  :tag "lisp"
  :added "2021-05-27"
  :ensure t
  :hook ((emacs-lisp-mode-hook . enable-paredit-mode)
         (lisp-mode-hook . enable-paredit-mode)
         (lisp-interacton-mode-hook . enable-paredit-mode)
         (scheme-mode-hook . enable-paredit-mode)))

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

(leaf sh-script
  :doc "shell-script editing commands for Emacs"
  :tag "builtin"
  :added "2021-10-24"
  :custom ((sh-basic-offset . 2)
           (sh-indentation . 2)))

(leaf google-c-style
  :doc "Google's C/C++ style for c-mode"
  :tag "tools" "c"
  :added "2021-10-26"
  :ensure t)

(leaf cc-mode
  :doc "major mode for editing C and similar languages"
  :tag "builtin"
  :defvar (c-basic-offset)
  :bind (c-mode-base-map
         ("C-c c" . compile))
  :after (google-set-c-style)
  :mode-hook
  (c-mode-hook . ((c-set-style "google-set-c-style")
                  (setq c-basic-offset 2))))

(leaf eglot
  :ensure t)

(leaf ruby-mode
  :custom (ruby-insert-encoding-magic-comment . nil)
  :config (add-hook 'ruby-mode-hook 'eglot-ensure))

(leaf ruby-electric
  :ensure t
  :hook (ruby-mode-hook 'ruby-electric-mode))

(leaf rubocop
  :ensure t
  :hook ((ruby-mode-hook . rubocop-mode)
         (lambda ()
           (setq flycheck-checker 'ruby-rubocop))))

(leaf projectile
  :ensure t
  :bind
  (projectile-mode-map
   ("s-p"   . projectile-command-map)
   ("M-p" . projectile-command-map))
  :init
  (projectile-mode)
  :hook ((projectile-mode-hook . projectile-rails-global-mode))
  :global-minor-mode projectile-mode)

(leaf projectile-rails
  :ensure t
  :after projectile
  :bind ("C-c r" . projectile-rails-command-map)
  :global-minor-mode projectile-rails-global-mode)

(leaf vue-mode
  :ensure t
  :hook (vue-mode-hook . lsp))

;;(use-package lsp-mode :commands lsp)
(leaf lsp-mode
  :doc "LSP mode"
  :req "emacs-26.1" "dash-2.18.0" "f-0.20.0" "ht-2.3" "spinner-1.7.3" "markdown-mode-2.3" "lv-0"
  :tag "languages" "emacs>=26.1"
  :url "https://github.com/emacs-lsp/lsp-mode"
  :added "2021-09-12"
  :emacs>= 26.1
  :ensure t
  :init (yas-global-mode)
  :after spinner markdown-mode lv
  :hook ((rust-mode-hook . lsp)
         (c-mode-hook . lsp)
         (c++-mode-hook . lsp))
  :config ((setq read-process-output-max
                 lsp-idle-delay 0.1)))

(leaf lsp-ui
  :doc "UI modules for lsp-mode"
  :req "emacs-26.1" "dash-2.18.0" "lsp-mode-6.0" "markdown-mode-2.3"
  :tag "tools" "languages" "emacs>=26.1"
  :url "https://github.com/emacs-lsp/lsp-ui"
  :added "2021-09-12"
  :emacs>= 26.1
  :ensure t
  :after lsp-mode markdown-mode
  :custom ((lsp-ui-flycheck-enable . t)))


(leaf ccls
  :doc "ccls client for lsp-mode"
  :req "emacs-25.1" "lsp-mode-6.3.1" "dash-2.14.1"
  :tag "c++" "lsp" "languages" "emacs>=25.1"
  :url "https://github.com/MaskRay/emacs-ccls"
  :added "2021-09-12"
  :emacs>= 25.1
  :ensure t
  :after lsp-mode
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp))))

(defun ccls/callee () (interactive) (lsp-ui-peek-find-custom "$ccls/call" '(:callee t)))
(defun ccls/caller () (interactive) (lsp-ui-peek-find-custom "$ccls/call"))
(defun ccls/vars (kind) (lsp-ui-peek-find-custom "$ccls/vars" `(:kind ,kind)))
(defun ccls/base (levels) (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels)))
(defun ccls/derived (levels) (lsp-ui-peek-find-custom "$ccls/inheritance" `(:levels ,levels :derived t)))
(defun ccls/member (kind) (interactive) (lsp-ui-peek-find-custom "$ccls/member" `(:kind ,kind)))

;; References w/ Role::Role
(defun ccls/references-read () (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
    (plist-put (lsp--text-document-position-params) :role 8)))

;; References w/ Role::Write
(defun ccls/references-write ()
  (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 16)))

;; References w/ Role::Dynamic bit (macro expansions)
(defun ccls/references-macro () (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
   (plist-put (lsp--text-document-position-params) :role 64)))

;; References w/o Role::Call bit (e.g. where functions are taken addresses)
(defun ccls/references-not-call () (interactive)
  (lsp-ui-peek-find-custom "textDocument/references"
                           (plist-put (lsp--text-document-position-params) :excludeRole 32)))


(leaf rainbow-delimiters
  :ensure t
  :hook
  ((prog-mode-hook . rainbow-delimiters-mode)))

(leaf elpy
  :ensure t
  :init
  (elpy-enable))

(leaf lsp-pyright
  :ensure t
  :hook (python-mode-hook . (lambda ()
                              (require 'lsp-pyright)
                              (lsp-deferred))))

;; Custom command.
(defun show-current-time ()
  "Show current time."
  (interactive)
  (message (current-time-string)))

(global-set-key (kbd "C-c t") 'show-current-time)
(global-set-key (kbd "C-c d") 'delete-trailing-whitespace)

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; init.el ends here
