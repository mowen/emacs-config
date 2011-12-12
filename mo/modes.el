;; ----------------------------------------
;; auto-complete

(add-to-list 'load-path (concat mo-vendor-dir "/auto-complete"))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (concat mo-vendor-dir "/auto-complete/dict"))
(ac-config-default)
;; TODO - find a better key for auto-complete
(define-key ac-mode-map (kbd "C-TAB") 'auto-complete)

;; ----------------------------------------
;; yasnippet

(eval-after-load 'yasnippet-bundle
  '(progn
    (yas/load-directory "~/.emacs.d/snippets")))

;; ----------------------------------------
;; prog-mode default settings

(add-hook 'prog-mode-hook
          '(lambda ()
            (linum-mode t)
            (capitalized-words-mode t)
            (flyspell-prog-mode)
            (local-set-key (kbd "C-'") 'indent-buffer)
            (local-set-key (kbd "C-.") 'mo-indent-defun)))

;; ----------------------------------------
;; Other mode settings

(load "mo/text")
(load "mo/dired")
(load "mo/ibuffer")
(load "mo/org")
(load "mo/erc")
(load "mo/lang/markup")
(load "mo/lang/javascript")
(load "mo/lang/ruby")
(load "mo/lang/clojure")
(load "mo/lang/lisp")
