;; ----------------------------------------
;; Text Mode

(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 
	  '(lambda ()
	     (abbrev-mode)
	     (if (and (buffer-file-name)
		      (string-match "\\.textile$" (buffer-file-name)))
		 (progn
		   ;; Turn on Textile Minor Mode for Textile buffers
		   (require 'textile-minor-mode)
		   (textile-minor-mode t)
		   (setq truncate-lines nil)))))