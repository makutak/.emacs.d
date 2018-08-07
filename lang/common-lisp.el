(cond
 ((file-exists-p (expand-file-name "~/.roswell/lisp/quicklisp/slime-helper.el"))
  (load (expand-file-name "~/.roswell/lisp/quicklisp/slime-helper.el")))
 ((file-exists-p (expand-file-name "~/quicklisp/slime-helper.el"))
  (load (expand-file-name "~/quicklisp/slime-helper.el")))
 ((file-exists-p (expand-file-name "~/.roswell/helper.el"))
  (load (expand-file-name "~/.roswell/helper.el")))
 (t (require-or-install 'slime)))

(defun slime-qlot-exec (directory)
  (interactive (list (read-directory-name "Project directory: ")))
  (slime-start :program "qlot"
               :program-args '("exec" "ros" "-S" "." "run")
               :directory directory
               :name 'qlot
               :env (list (concat "PATH="
                                  (mapconcat 'identity exec-path ":"))
                          (concat "QUICKLISP_HOME="
                                  (file-name-as-directory directory) "quicklisp/"))))

(defun lisp-hook-fn ()
  (interactive)
  ;; Start slime mode
  (slime-mode)
  ;; Some useful key-bindings
  (local-set-key [tab] 'slime-complete-symbol)
  (local-set-key (kbd "M-q") 'slime-reindent-defun)
  ;; We set the indent function. common-lisp-indent-function
  ;; will indent our code the right way
  (set (make-local-variable lisp-indent-function) 'common-lisp-indent-function)
  ;; We tell slime to not load failed compiled code
  (setq slime-load-failed-fasl 'never))
;; Finally we tell lisp-mode to run our function on startup
(add-hook 'lisp-mode-hook 'lisp-hook-fn)
