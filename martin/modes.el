(load "martin/text")
(load "martin/dired")
(load "martin/ibuffer")
(load "martin/org")
(load "martin/erc")
(load "martin/lang/markup")
(load "martin/lang/javascript")
(load "martin/lang/php")
(load "martin/lang/ruby")
(load "martin/lang/clojure")
(load "martin/lang/lisp")
(load (concat mo-vendor-dir "/gist/gist"))

;; yasnippet
(load (concat mo-vendor-dir "/yasnippet/yasnippet"))
(setq yas/snippet-dirs (concat mo-vendor-dir "/yasnippet/snippets"))
(yas/load-directory yas/snippet-dirs)
(yas/global-mode) ;; Enable yasnippet minor mode in ALL buffers