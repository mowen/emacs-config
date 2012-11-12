;; ----------------------------------------
;; yasnippet

(eval-after-load 'yasnippet-bundle
  '(progn
    (yas/load-directory "~/.emacs.d/snippets")))

;; ----------------------------------------
;; prog-mode default settings

(add-hook 'prog-mode-hook
          '(lambda ()
            (hl-line-mode -1)
            (capitalized-words-mode t)
            (flyspell-prog-mode) ;; Only highlight typos in strings and comments
            ;;(linum-mode t)
            ;;(local-set-key (kbd "C-.") 'esk-cleanup-buffer)
            (local-set-key (kbd "C-'") 'esk-cleanup-buffer)))

(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

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
