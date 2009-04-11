;; ----------------------------------------
;; iBuffer
;; ----------------------------------------

(setq ibuffer-expert t) ;; Don't ask for confirmation when deleting buffers
(setq ibuffer-saved-filter-groups
      '(("home"
	 ("Subversion" (name . "\*svn"))
	 ("Magit" (mode . magit-mode))
	 ("Org" (or (mode . org-mode) (name . "OrgMode")))
	 ("ERC" (mode . erc-mode))
	 ("emacs-config" (filename . ".emacs.d"))
	 ("Web Dev" (or (mode . html-mode)
			(mode . css-mode)))
	 ("Help" (or (name . "\*Help\*")
		     (name . "\*Apropos\*")
		     (name . "\*info\*"))))
	("work"
	 ("Org" (or (mode . org-mode) (name . "OrgMode")))
	 ("Train" (filename . "^n:/"))
	 ("Dev" (filename . "^v:/"))
	 ("PBMS" (filename . "wwwroot/PBMS"))
	 ("SeftonMayrise" (filename . "wwwroot/SeftonMayrise"))
	 ("SeftonEIP" (filename . "wwwroot/SeftonEIP"))
	 ("ReleaseMe" (filename . "ReleaseMe"))
	 ("Subversion" (name . "\*svn"))
	 ("Help" (or (name . "\*Help\*")
		     (name . "\*Apropos\*")
		     (name . "\*info\*"))))))

(add-hook 'ibuffer-mode-hook 
	  '(lambda ()
	     (ibuffer-switch-to-saved-filter-groups (symbol-name mo-location))))