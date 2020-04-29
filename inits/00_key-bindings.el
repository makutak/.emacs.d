(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map "\C-z" 'undo)
(define-key global-map "\C-h" 'backward-delete-char)
(define-key global-map (kbd "C-2") 'set-mark-command)

;; Shift + 矢印でウィンドウを移動する
(windmove-default-keybindings)

;; 文末のスペースを削除する
(defun remove_space ()
  (interactive)
  (setq start (point-min))
  (setq end (point-max))
  (save-excursion
    (save-restriction
      (while (re-search-forward "[ \t]+\n" nil t)
        (replace-match "\n")))))
(global-set-key "\C-c\C-x" 'remove_space)

;; 改行コードをCRLFからLFに置換し、ファイル全体をインデントする
(defun replace-crlf-and-indent ()
  (interactive)
  (setq start (point-min))
  (setq end (point-max))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (re-search-forward "" nil t)
        (replace-match ""))
      (goto-char start)
      (while (re-search-forward "[ \t]+\n" nil t)
        (replace-match "\n"))))
  (goto-char start)
  (indent-region start end))
(global-set-key "\C-c\C-c" 'replace-crlf-and-indent)

;; ファイル全体をインデントし、タブをスペースに変換する
(defun indent-all ()
  (interactive)
  (setq start (point-min))
  (setq end (point-max))
  (goto-char start)
  (untabify start end)
  (indent-region start end))
(global-set-key "\C-c\C-v" 'indent-all)

(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (dotimes (i (- num_wins 1))
    (split-window-horizontally))
  (balance-windows))

;;画面を三分割
(global-set-key "\C-x@" (lambda ()
                          (interactive)
                          (split-window-horizontally-n 3)))
