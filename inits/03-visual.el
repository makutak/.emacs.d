(show-paren-mode t)

(when (windowsp)
  (set-default-font "Consolas 12"))

(require-or-install 'whitespace)
;; (set-face-foreground 'whitespace-space "DarkGoldenrod1")
;; (set-face-background 'whitespace-space nil)
;; (set-face-bold-p 'whitespace-space t)
;; (set-face-foreground 'whitespace-tab "DarkOliveGreen1")
;; (set-face-background 'whitespace-tab nil)
;; (set-face-underline 'whitespace-tab t)
;; (setq whitespace-style '(face tabs tab-mark spaces space-mark))
;; (setq whitespace-space-regexp "\\(\x3000+\\)")
;; (setq whitespace-display-mappings
;;        '((space-mark ?\x3000 [?\□])
;;          (tab-mark ?\t [?\xBB ?\t])))

(setq whitespace-style '(face           ; faceで可視化
                         trailing       ; 行末
                         tabs           ; タブ
                         spaces         ; スペース
                         empty          ; 先頭/末尾の空行
                         space-mark     ; 表示のマッピング
                         tab-mark
                         ))

(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please, comment the line below.
        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

;; スペースは全角のみを可視化
(setq whitespace-space-regexp "\\(\u3000+\\)")

;; 保存前に自動でクリーンアップ
(setq whitespace-action '(auto-cleanup))

(global-whitespace-mode 1)

(defvar my/bg-color "#232323")
(set-face-attribute 'whitespace-trailing nil
                    :background my/bg-color
                    :foreground "DeepPink"
                    :underline t)
(set-face-attribute 'whitespace-tab nil
                    :background my/bg-color
                    :foreground "LightSkyBlue"
                    :underline t)
(set-face-attribute 'whitespace-space nil
                    :background my/bg-color
                    :foreground "GreenYellow"
                    :weight 'bold)
(set-face-attribute 'whitespace-empty nil
                    :background my/bg-color)
(global-whitespace-mode 1)

(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background "ForestGreen"))
    (t
     ()))
  nil :group 'font-lock-highlighting-faces)
(setq hl-line-face 'hlline-face)
(global-hl-line-mode)
