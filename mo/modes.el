;; yasnippet
(load (concat mo-vendor-dir "/yasnippet/yasnippet"))
(setq yas/snippet-dirs (concat mo-vendor-dir "/yasnippet/snippets"))
(yas/load-directory yas/snippet-dirs)
(yas/global-mode) ;; Enable yasnippet minor mode in ALL buffers

;; auto-complete
(add-to-list 'load-path (concat mo-vendor-dir "/auto-complete"))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat mo-vendor-dir "/auto-complete/dict"))
(ac-config-default)
;; TODO - find a better key for auto-complete
(define-key ac-mode-map (kbd "C-TAB") 'auto-complete)

;; ace-jump-mode
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(load "mo/text")
(load "mo/dired")
(load "mo/ibuffer")
(load "mo/org")
(load "mo/erc")
(load "mo/eproject")
(load "mo/lang/markup")
(load "mo/lang/javascript")
(load "mo/lang/php")
(load "mo/lang/ruby")
(load "mo/lang/clojure")
(load "mo/lang/lisp")
(load (concat mo-vendor-dir "/gist/gist"))
