(add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))
(setq org-startup-indented t)
;; 画像をインラインで表示
(setq org-startup-with-inline-images t)
;; 見出しの余分な*を消す
(setq org-hide-leading-stars t)
;; LOGBOOK drawerに時間を格納する
(setq org-clock-into-drawer t)
;; DONEの時刻を記録
(setq org-log-done 'time)
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "SOMEDAY(s)")))
