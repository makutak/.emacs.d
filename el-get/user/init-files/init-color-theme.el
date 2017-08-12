(require 'color-theme-tangotango)
(color-theme-tangotango)

(defvar paren-face 'paren-face)
(make-face 'paren-face)
(set-face-foreground 'paren-face "#ffffff")

(dolist (mode '(lisp-mode
                emacs-lisp-mode
                scheme-mode
                clojure-mode))
  (font-lock-add-keywords mode
                          '(("(\\|)" . paren-face))))
