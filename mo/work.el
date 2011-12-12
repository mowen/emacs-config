(load "mo/work/util")

(global-hl-line-mode -1) ;; Don't highlight the current line

;; Don't load Flyspell in work, as I haven't got around to installing
;; aspell
(add-hook 'after-init-hook
          '(lambda ()
             (remove-hook 'text-mode-hook 'turn-on-flyspell)
             (remove-hook 'prog-mode-hook 'flyspell-prog-mode)))

