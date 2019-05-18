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
     ((t (:foreground "#759FCD"))))             ; 要素名
   '(web-mode-html-attr-name-face
     ((t (:foreground "#C4734C"))))                          ; 属性名など
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
  :straight t)
