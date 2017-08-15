(require-or-install 'web-mode)

(setq web-mode-auto-close-style 1)
(setq web-mode-tag-auto-close-style t)

(appendf auto-mode-alist
         '(("\\.\\(html?\\|emb\\|tmpl\\|tt\\)$" . web-mode)
           ("\\.html$" . web-mode)
           ("\\.jsx$" . web-mode)
           ("\\.erb$" . web-mode)))

