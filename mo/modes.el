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
            (local-set-key (kbd "C-'") 'indent-buffer)
            (local-set-key (kbd "C-.") 'mo-indent-defun)))

(add-hook 'prog-mode-hook 'flyspell-prog-mode)

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
