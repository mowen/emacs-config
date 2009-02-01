;; ----------------------------------------
;; iBuffer
;; ----------------------------------------

(setq ibuffer-expert t) ;; Don't ask for confirmation when deleting buffers
(setq ibuffer-saved-filter-groups
      '(("home"
	 ("Org" (or (mode . org-mode) (name . "OrgMode")))
	 ("Subversion" (name . "\*svn"))
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