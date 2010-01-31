;; ----------------------------------------
;; iBuffer
;; ----------------------------------------

(setq ibuffer-expert t) ;; Don't ask for confirmation when deleting buffers
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("home"
	 ("Subversion" (name . "\*svn"))
	 ("Magit" (mode . magit-mode))
	 ("Org" (or (mode . org-mode) (name . "OrgMode")))
	 ("ERC" (mode . erc-mode))
	 ("emacs-config" (filename . ".emacs.d"))
         ("martinowen.net" (filename . "martinowen.net"))
         ("code" (filename . "code"))
	 ("Web Dev" (or (mode . html-mode)
			(mode . css-mode)))
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
	     (ibuffer-auto-mode 1)
	     (ibuffer-switch-to-saved-filter-groups (symbol-name mo-location))))