(defun scss-custom ()
  (setq indent-tabs-mode nil)
  (setq css-indent-offset 2))


(use-package scss-mode
  :straight t
  :init
  (setq css-indent-offset 2)
  (setq-default indent-tabs-mode nil))

(use-package vue-mode
  :straight t
  :hook ((web-mode . flycheck-mode)))

;; (use-package web-mode
;;   :straight t
;;   :mode (("\\.scss$" . web-mode))
;;   :config
;;   (setq web-mode-cssindent-offset 2)
;;   (setq web-mode-scssindent-offset 2)
;;   (setq web-mode-script-padding 0)
;;   (setq web-mode-style-padding 0)
;;   (setq web-mode-markup-indent-offset 2))
