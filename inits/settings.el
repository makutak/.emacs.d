(defun windowsp ()
  (eq system-type 'windows-nt))
(defun mac-os-p ()
  ;; (member window-system '(mac ns))
  (eq system-type 'darwin))
(defun linuxp ()
  (eq window-system 'x))

(defmacro appendf (list &rest lists)
  `(setq ,list (append ,list ,@lists)))

(setq x-select-enable-clipboard t)

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
