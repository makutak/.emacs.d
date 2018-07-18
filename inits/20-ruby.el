(require-or-install 'ruby-mode)
(require-or-install 'ruby-block)
;;(require-or-install 'ruby-electric)
(require-or-install 'ruby-end)
(require-or-install 'inf-ruby)

;;カッコの自動挿入
(electric-pair-mode t)
(add-to-list 'electric-pair-pairs '(?| . ?|))

;;magic comment は不要
(custom-set-variables
 '(ruby-insert-encoding-magic-comment nil))

;; Gemfile for ruby-mode
;; .gemspec for ruby-mode
;; Capfile for ruby-mode
;; Guardfile for ruby-mode
(appendf  auto-mode-alist
          '(("Gemfile$" . ruby-mode)
            ("\\.gemspec$" . ruby-mode)
            ("Capfile$" . ruby-mode)
            ("Guardfile$" . ruby-mode)))

;;inf-rubyでpryを使う
(setq inf-ruby-default-implementation "pry")
(setq inf-ruby-eval-binding "Pry.toplevel_binding")
(add-hook 'inf-ruby-mode-hook 'ansi-color-for-comint-mode-on)

(defadvice ruby-indent-line (after unindent-closing-paren activate)
  (let ((column (current-column))
        indent offset)
    (save-excursion
      (back-to-indentation)
      (let ((state (syntax-ppss)))
        (setq offset (- column (current-column)))
        (when (and (eq (char-after) ?\))
                   (not (zerop (car state))))
          (goto-char (cadr state))
          (setq indent (current-indentation)))))
    (when indent
      (indent-line-to indent)
      (when (> offset 0) (forward-char offset)))))
