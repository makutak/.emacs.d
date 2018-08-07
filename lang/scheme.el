(require-or-install 'geiser)
(setq geiser-chez-binary "~/.scheme-env/implementations/chez/v9.5/bin/scheme")
;;(setq geiser-active-implementations '(guile))
;;(setq geiser-active-implementations '(chez))
(setq geiser-repl-history-filename "~/.emacs.d/geiser-history")

;; UTF-8 に統一
(setq process-coding-system-alist
      (cons '("gosh" utf-8 . utf-8) process-coding-system-alist))

(setq scheme-program-name "gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

;; 別のウィンドウに gosh を動作させる
(defun scheme-other-window ()
  "Run Gauche on other window"
  (interactive)
  (split-window-horizontally (/ (frame-width) 2))
  (let ((buf-name (buffer-name (current-buffer))))
    (scheme-mode)
    (switch-to-buffer-other-window
     (get-buffer-create "*scheme*"))
    (run-scheme scheme-program-name)
    (switch-to-buffer-other-window
     (get-buffer-create buf-name))))

(define-key global-map "\C-cS" 'scheme-other-window)
