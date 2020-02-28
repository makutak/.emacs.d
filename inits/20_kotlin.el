(add-to-list 'exec-path "~/work/kotlin-language-server/server/build/install/server/bin")

(use-package kotlin-mode)
(use-package flycheck-kotlin
	:after kotlin-mode
	:init
	(flycheck-kotlin-setup))
