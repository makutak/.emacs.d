;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs

;;; Code:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package init-loader)
(load-file "~/.emacs.d/inits/settings.el")
(load-file "~/.emacs.d/inits/key-bindings.el")
(load-file "~/.emacs.d/inits/ui.el")
(load-file "~/.emacs.d/inits/utils.el")
(load-file "~/.emacs.d/inits/typescript.el")
(load-file "~/.emacs.d/inits/web.el")
(load-file "~/.emacs.d/inits/clojure.el")
(provide 'init)
;;; init.el ends here
