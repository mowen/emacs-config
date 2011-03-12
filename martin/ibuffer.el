;; ----------------------------------------
;; iBuffer
;; ----------------------------------------

(setq ibuffer-expert t) ;; Don't ask for confirmation when deleting buffers
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-saved-filter-groups
      '(("home"
	 ("emacs-config" (or (filename . ".emacs.d")
			     (filename . "emacs-config")))
         ("martinowen.net" (filename . "martinowen.net"))
	 ("Org" (or (mode . org-mode)
		    (filename . "OrgMode")))
	 ("code-lisp" (filename . "code/lisp"))
	 ("code-ruby" (filename . "code/ruby"))
	 ("code-javascript" (filename . "code/javascript"))
	 ("code-mongodb" (filename . "code/mongodb"))
	 ("code-websites" (filename . "code/websites"))
         ("code" (filename . "code"))
	 ("Web Dev" (or (mode . html-mode)
			(mode . css-mode)))
	 ("Subversion" (name . "\*svn"))
	 ("Magit" (name . "\*magit"))
	 ("ERC" (mode . erc-mode))
	 ("Help" (or (name . "\*Help\*")
		     (name . "\*Apropos\*")
		     (name . "\*info\*"))))
	("work"
	 ("Qore3 UI Views" (filename . "Qore3/src/Qire.Qore.UI/qore/views"))
	 ("Qore3 UI Controllers" (filename . "Qore3/src/Qire.Qore.UI/qore/controllers"))
	 ("Qore3 UI Models" (filename . "Qore3/src/Qire.Qore.UI/qore/models"))
	 ("Qore3 UI" (filename . "Qore3/src/Qire.Qore.UI"))
	 ("Qore3" (filename . "Qore3"))
	 ("QirePay" (filename . "Projects/QirePay"))
	 ("Qire-Qore" (filename . "Projects/Qire-Qore-Main-Source"))
	 ("Mongo DB" (or (filename . "mongodb")
			 (filename . "MongoDB")))
	 ("Visual Studio" (or (filename . "Visual Studio 2010")
			      (filename . "Visual Studio 2008")))
	 ("emacs-config" (filename . ".emacs.d"))
	 ("emacs" (filename . "Program Files/emacs-23.2"))
	 ("desktop" (filename . "Desktop"))
	 ("Magit" (name . "\*magit"))
	 ("SVN" (name . "\*svn"))
	 ("ERC" (mode . erc-mode))
	 ("JavaScript" (mode . espresso-mode))
	 ("Code" (filename . "Code"))
	 ("Org" (or (mode . org-mode)
		    (filename . "OrgMode")
		    (filename . "HomeOrg")))
	 ("Help" (or (name . "\*Help\*")
		     (name . "\*Apropos\*")
		     (name . "\*info\*"))))))

;; Enable ibuffer-filter-by-filename to filter on directory names too.
(eval-after-load "ibuf-ext"
  '(define-ibuffer-filter filename
     "Toggle current view to buffers with file or directory name matching QUALIFIER."
     (:description "filename"
		   :reader (read-from-minibuffer "Filter by file/directory name (regexp): "))
     (ibuffer-awhen (or (buffer-local-value 'buffer-file-name buf)
			(buffer-local-value 'dired-directory buf))
		    (string-match qualifier it))))

(add-hook 'ibuffer-mode-hook 
  	  '(lambda ()
 	     (ibuffer-auto-mode 1)
 	     (ibuffer-switch-to-saved-filter-groups (symbol-name mo-location))))