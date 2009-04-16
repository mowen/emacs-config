;; ----------------------------------------
;; NXHTML Mode

(load (concat mo-vendor-dir "/nxhtml/autostart.el"))


;; (load (concat site-lisp "nxml-mode-20041004/rng-auto.el"))
;; (add-to-list 'auto-mode-alist 
;; 	     '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|config\\|vbproj\\)\\'" . nxml-mode))

;; ----------------------------------------
;; XML Mode
;; ----------------------------------------

(defun mo-nxml-mark-node ()
  "Mark the node contained on the current line."
  (interactive)
  (beginning-of-line)
  (setq beg (point))
  (nxml-forward-element)
  (setq end (point))
  (push-mark beg)
  (goto-char end)
  (exchange-point-and-mark))

;; (add-to-list 'auto-mode-alist 
;; 	     '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|config\\|vbproj\\)\\'" . nxml-mode))
(add-hook 'nxml-mode-hook 
 	  '(lambda () 
 	     (setq nxml-slash-auto-complete-flag t)
	     (define-key nxml-mode-map (kbd "M-h") 'mo-nxml-mark-node)
	     (if (eq mo-location 'work)
		 (define-key nxml-mode-map (kbd "C-c C-r") 'fo-remove-xml-attribute-region))))
