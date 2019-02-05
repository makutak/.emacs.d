(defun web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-cssindent-offset 2)
  (setq web-mode-scssindent-offset 2)
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0))


(defun scss-custom ()
  (setq indent-tabs-mode nil)
  (setq css-indent-offset 2))


(use-package scss-mode
  :straight t
  :init
  (setq css-indent-offset 2)
  (setq-default indent-tabs-mode nil))

(use-package web-mode
  :straight t
  :hook
  (web-mode . web-mode-hook)
  :config
  (custom-set-faces
   '(web-mode-doctype-face
     ((t (:foreground "#82AE46"))))                          ; doctype
   '(web-mode-html-tag-face
     ((t (:foreground "#897DD1"))))             ; 要素名
   '(web-mode-html-attr-name-face
     ((t (:foreground "#88CF57"))))                          ; 属性名など
   '(web-mode-html-attr-value-face
     ((t (:foreground "#5EAFAF"))))                          ; 属性値
   '(web-mode-comment-face
     ((t (:foreground "#D9333F"))))                          ; コメント
   '(web-mode-server-comment-face
     ((t (:foreground "#D9333F"))))                          ; コメント
   '(web-mode-css-rule-face
     ((t (:foreground "#A0D8EF"))))                          ; cssのタグ
   '(web-mode-css-pseudo-class-face
     ((t (:foreground "#FF7F00"))))                          ; css 疑似クラス
   '(web-mode-css-at-rule-face
     ((t (:foreground "#FF7F00"))))) ; cssのタグ
  )

(use-package vue-mode
  :straight t
  :hook (vue-mode . web-mode-hook)
  :config
  (custom-set-variables
   ;; custom-set-variables was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(vue-modes
     (quote
      ((:type template :name nil :mode web-mode)
       (:type template :name html :mode web-mode)
       (:type template :name jade :mode jade-mode)
       (:type template :name pug :mode pug-mode)
       (:type template :name slm :mode slim-mode)
       (:type template :name slim :mode slim-mode)
       (:type script :name nil :mode js-mode)
       (:type script :name js :mode web-mode)
       (:type script :name es6 :mode js-mode)
       (:type script :name babel :mode js-mode)
       (:type script :name coffee :mode coffee-mode)
       (:type script :name ts :mode typescript-mode)
       (:type script :name typescript :mode typescript-mode)
       (:type style :name nil :mode web-mode)
       (:type style :name css :mode web-mode)
       (:type style :name stylus :mode stylus-mode)
       (:type style :name less :mode less-css-mode)
       (:type style :name postcss :mode css-mode)
       (:type style :name scss :mode scss-mode)
       (:type style :name sass :mode ssass-mode)))))
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   ))
