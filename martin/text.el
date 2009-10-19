;; ----------------------------------------
;; Text Mode

(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 
	  '(lambda ()
	     (abbrev-mode)
	     (turn-on-auto-fill)
	     (if (string-match "\\.textile$" (buffer-file-name))
		 (progn
		   ;; Turn on Textile Minor Mode for Textile buffers
		   (require 'textile-minor-mode)
		   (textile-minor-mode)))))