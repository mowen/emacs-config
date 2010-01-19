(setq mo-work-site-lisp "C:/Program Files/emacs-23.1/site-lisp")
(add-to-list 'load-path mo-work-site-lisp)

(load "martin/work/skeletons")
(load "martin/work/util")
(load "martin/work/front-office/front-office-mode")
(load "martin/work/front-office/front-office-util")
(load "martin/work/front-office/front-office-email-mode")

(global-hl-line-mode -1) ;; Don't highlight the current line

(add-hook 'nxml-mode-hook
	  '(lambda ()
	     ;; Load Front Office Email Minor Mode if the file is in EmailTemplates.
	     (if (string-match "App_Data/EmailTemplates" (buffer-file-name))
		 (fo3-email-mode))))

(add-to-list 'auto-mode-alist '("mail.*\\.txt" . front-office-mode))