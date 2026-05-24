;;; early-init.el --- Early startup configuration -*- lexical-binding: t; -*-

;; 初期フレームの表示前に UI を設定する。
(load-theme 'tsdh-dark)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;;; early-init.el ends here
