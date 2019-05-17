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
  :mode (("\\.html?\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.jsx\\'" . web-mode))
  :hook
  (web-mode . web-mode-hook)
  :config
  (setq typescript-indent-level 2)
  (custom-set-faces
   '(web-mode-doctype-face
     ((t (:foreground "#82AE46"))))     ; doctype
   '(web-mode-html-tag-face
     ((t (:foreground "#759FCD"))))     ; 要素名
   '(web-mode-html-attr-name-face
     ((t (:foreground "#C4734C"))))     ; 属性名など
   '(web-mode-html-attr-value-face
     ((t (:foreground "#5EAFAF"))))     ; 属性値
   '(web-mode-comment-face
     ((t (:foreground "#D9333F"))))     ; コメント
   '(web-mode-server-comment-face
     ((t (:foreground "#D9333F"))))     ; コメント
   '(web-mode-css-rule-face
     ((t (:foreground "#A0D8EF"))))     ; cssのタグ
   '(web-mode-css-pseudo-class-face
     ((t (:foreground "#FF7F00"))))     ; css 疑似クラス
   '(web-mode-css-at-rule-face
     ((t (:foreground "#FF7F00"))))) ; cssのタグ
  )

(use-package vue-mode
  :straight t)

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

(use-package typescript-mode)

(use-package tide
  :straight t
  :config
  (setq typescript-indent-level 4)
  (setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions
                              t
                              :placeOpenBraceOnNewLineForFunctions
                              nil))
  :after (typescript-mode company flycheck)
  :hook
  (typescript-mode . tide-setup)
  (typescript-mode . tide-hl-identifier-mode)
  (before-save . tide-format-before-save))


(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)
