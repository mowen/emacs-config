(load "martin/work/util")

(global-hl-line-mode -1) ;; Don't highlight the current line

;; Don't load Flyspell in work, as I haven't got around to installing aspell 
(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(remove-hook 'text-mode-hook 'turn-on-flyspell)
