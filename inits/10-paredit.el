(loop for mode in '(emacs-lisp-mode
                    lisp-interacton-mode
                    lisp-mode
                    scheme-mode
                    slime-repl-mode
                    clojure-mode
                    cider-repl-mode
                    ruby-mode)
      do (add-hook (intern (concat (symbol-name mode) "-hook"))
                   (lambda ()
                     (require-or-install 'paredit)
                     (paredit-mode +1))))

(eval-after-load "paredit"
  '(progn
     (define-key paredit-mode-map "[" 'paredit-open-bracket)
     (define-key paredit-mode-map "]" 'paredit-close-bracket)
     (define-key paredit-mode-map "(" 'paredit-open-parenthesis)
     (define-key paredit-mode-map ")" 'paredit-close-parenthesis)
     (define-key paredit-mode-map "{" 'paredit-open-curly)
     (define-key paredit-mode-map "}" 'paredit-close-curly)))
