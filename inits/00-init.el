(defun windowsp ()
  (eq system-type 'windows-nt))
(defun mac-os-p ()
  ;; (member window-system '(mac ns))
  (eq system-type 'darwin))
(defun linuxp ()
  (eq window-system 'x))

(defmacro appendf (list &rest lists)
  `(setq ,list (append ,list ,@lists)))

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


(fset 'yes-or-no-p 'y-or-n-p)

;;（改行、）タブ、スペースに色を付ける
(defface return-color '((t (:background "DarkGray"))) nil)
(defface tab-color '((t (:background "Gray"))) nil)
(defface full-width-space-color '((t (:background "Green"))) nil)
(defface half-width-space-color '((t (:background "Blue"))) nil)
(defvar return-color 'return-color)
(defvar tab-color 'tab-color)
(defvar full-width-space-color 'full-width-space-color)
(defvar half-width-space-color 'half-width-space-color)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords
   major-mode
   '(("\t" 0 tab-color append)
     ("　" 0 full-width-space-color append)
     ("[ \t]+$" 0 half-width-space-color append)
     ;;("[\r]*\n" 0 return append)
     )))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)

;; auto-install
(require 'auto-install)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/auto-install/"))
