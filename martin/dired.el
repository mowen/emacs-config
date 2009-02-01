;; ----------------------------------------
;; Dired Mode

(add-hook 'dired-mode-hook
	  '(lambda ()
	     (define-key dired-mode-map (kbd "C-c C-s")
	       '(lambda ()
		  (interactive)
		  (svn-status default-directory)))))

;; ----------------------------------------
;; TRAMP Setup

(require 'tramp)
(setq tramp-auto-save-directory (expand-file-name "~/tmp"))
(setq tramp-default-method "plink")
