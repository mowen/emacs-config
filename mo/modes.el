;; ----------------------------------------
;; prog-mode default settings

(defun mo-prog-mode-hook ()
  (hl-line-mode -1)
  (auto-fill-mode -1)
  (capitalized-words-mode t)
  (local-set-key (kbd "C-'") 'esk-cleanup-buffer))

(add-hook 'prog-mode-hook 'mo-prog-mode-hook)

(remove-hook 'prog-mode-hook 'esk-local-comment-auto-fill)
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
