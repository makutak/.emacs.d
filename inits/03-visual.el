(show-paren-mode t)

(when (windowsp)
  (set-default-font "Consolas 12"))

(require-or-install 'whitespace)
(set-face-foreground 'whitespace-space "DarkGoldenrod1")
(set-face-background 'whitespace-space nil)
(set-face-bold-p 'whitespace-space t)
(set-face-foreground 'whitespace-tab "DarkOliveGreen1")
(set-face-background 'whitespace-tab nil)
(set-face-underline 'whitespace-tab t)
(setq whitespace-style '(face tabs tab-mark spaces space-mark))
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
       '((space-mark ?\x3000 [?\□])
         (tab-mark ?\t [?\xBB ?\t])))

;; 保存前に自動でクリーンアップ
(setq whitespace-action '(auto-cleanup))

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
