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

(mapc 'mo-load '(text dired ibuffer org erc
                 lang/markup lang/javascript lang/ruby
                 lang/coffeescript lang/clojure lang/lisp))
