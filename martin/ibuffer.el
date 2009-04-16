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
	 ("Subversion" (name . "\*svn"))
	 ("Org" (or (mode . org-mode) (name . "OrgMode")))
	 ("FO2 Dev" (filename . "^v:/"))
	 ("FO2 Train" (filename . "^n:/"))
	 ("FO2 Live" (filename . "^o:/"))
	 ("FO3 Workbench" (filename . "frontoffice30/organisations/smbc/WorkBench"))
	 ("FO3 Front Office Web" (filename . "frontoffice30/organisations/smbc/FrontOfficeWeb"))
	 ("Mayrise - Tech Services" (filename . "wwwroot/SeftonMayriseTechServices"))
	 ("Mayrise - Refuse" (filename . "wwwroot/SeftonMayriseRefuse"))
	 ("SeftonEIP" (filename . "wwwroot/SeftonEIP"))
	 ("emacs-config" (filename . ".emacs.d"))
	 ("Help" (or (name . "\*Help\*")
		     (name . "\*Apropos\*")
		     (name . "\*info\*"))))))

(add-hook 'ibuffer-mode-hook 
	  '(lambda ()
	     (ibuffer-switch-to-saved-filter-groups (symbol-name mo-location))))