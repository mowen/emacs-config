;; ----------------------------------------
;; NXHTML Mode

(if (eq mo-location 'work)
    (load (concat mo-work-site-lisp "/nxhtml/autostart.el"))
  (load (concat mo-vendor-dir "/nxhtml/autostart.el")))

;; (load (concat site-lisp "nxml-mode-20041004/rng-auto.el"))
;; (add-to-list 'auto-mode-alist 
;; 	     '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|config\\|vbproj\\)\\'" . nxml-mode))

;; ----------------------------------------
;; XML Mode
;; ----------------------------------------

(defun mo-nxml-indent-buffer ()
  "Indent the whole buffer correctly."
  (interactive)
  (let ((xml (xml-parse-region (point-min) (point-max) (current-buffer))))
    (save-excursion
      (erase-buffer)
      (goto-char (point-min))
      (insert (xml-print xml)))))

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
	     (define-key nxml-mode-map (kbd "C-c i") 'mo-nxml-indent-buffer)
	     (if (eq mo-location 'work)
		 (define-key nxml-mode-map (kbd "C-c C-r") 'fo-remove-xml-attribute-region))))
