;; ----------------------------------------
;; Text Mode

(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 
	  '(lambda ()
	     (abbrev-mode)
	     (turn-on-auto-fill)))