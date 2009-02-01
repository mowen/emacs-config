;; ----------------------------------------
;; XML Mode
;; ----------------------------------------

;; (load (concat site-lisp "nxml-mode-20041004/rng-auto.el"))
;; (add-to-list 'auto-mode-alist 
;; 	     '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|config\\|vbproj\\)\\'" . nxml-mode))
;; (add-hook 'nxml-mode-hook 
;; 	  '(lambda () 
;; 	     (setq nxml-slash-auto-complete-flag t)
;; 	     (define-key nxml-mode-map (kbd "C-c C-r") 'fo-remove-xml-attribute-region)))

;; (load (concat site-lisp "nxml/autostart.el"))
