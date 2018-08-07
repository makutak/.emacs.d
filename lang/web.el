(require-or-install 'web-mode)

(setq web-mode-enable-auto-closing t)
(setq web-mode-enable-auto-pairing t)

(setq web-mode-auto-close-style 2)
(setq web-mode-tag-auto-close-style 2)

(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(setq web-mode-script-padding 0)
(setq web-mode-style-padding 0)

(appendf auto-mode-alist
         '(("\\.\\(html?\\|emb\\|tmpl\\|tt\\)$" . web-mode)
           ("\\.html$" . web-mode)
           ("\\.jsx$" . web-mode)
           ("\\.erb$" . web-mode)
           ("\\.vue$" . web-mode)))
