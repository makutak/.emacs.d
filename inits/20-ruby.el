(require-or-install 'ruby-mode)
(require-or-install 'ruby-block)
(require-or-install 'ruby-electric)
(require-or-install 'ruby-end)

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

(add-hook 'ruby-mode-hook (lambda ()
                            (add-to-list (make-local-variable 'paredit-space-for-delimiter-predicates)
                                         (lambda (_ _) nil))
                            (enable-paredit-mode)))
