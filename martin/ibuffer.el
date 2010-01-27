;; ----------------------------------------
;; iBuffer
;; ----------------------------------------

(setq ibuffer-expert t) ;; Don't ask for confirmation when deleting buffers
(setq ibuffer-saved-filter-groups
      '(("home"
         ("martinowen.net" (filename . "martinowen.net"))
	 ("emacs-config" (filename . ".emacs.d"))
	 ("Org" (or (mode . org-mode) (filename . "OrgMode")))
         ("code" (filename . "code"))
	 ("Web Dev" (or (mode . html-mode)
			(mode . css-mode)))
	 ("ERC" (mode . erc-mode))
	 ("Subversion" (name . "\*svn"))
	 ("Magit" (name . "\*magit"))
	 ("Help" (or (name . "\*Help\*")
		     (name . "\*Apropos\*")
		     (name . "\*info\*"))))
	("work"
	 ("FO3 Working Copy" (filename . "Subversion Working Copies"))
	 ("FO3 Workbench" (filename . "frontoffice30/organisations/smbc/WorkBench"))
	 ("FO3 Front Office Web" (filename . "frontoffice30/organisations/smbc/FrontOfficeWeb"))
	 ("FO3 SeftonV3Utils" (filename . "wwwroot/SeftonV3Utils"))
	 ("Mayrise - Tech Services" (filename . "wwwroot/SeftonMayriseTechServices"))
	 ("Mayrise - Refuse" (filename . "wwwroot/SeftonMayriseRefuse"))
	 ("SeftonEIP" (filename . "wwwroot/SeftonEIP"))
	 ("emacs-config" (filename . ".emacs.d"))
	 ("Org" (or (mode . org-mode)
		    (filename . "OrgMode")
		    (filename . "HomeOrg")))
	 ("SVN" (name . "\*svn"))
	 ("Help" (or (name . "\*Help\*")
		     (name . "\*Apropos\*")
		     (name . "\*info\*"))))))

(add-hook 'ibuffer-mode-hook 
	  '(lambda ()
	     (ibuffer-switch-to-saved-filter-groups (symbol-name mo-location))))