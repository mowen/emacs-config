;; ----------------------------------------
;; NXHTML Mode

(load (concat mo-vendor-dir "/nxhtml/autostart.el"))

(add-to-list 'auto-mode-alist '("\\.ejs$" . nxhtml-mumamo-mode))

;; (load (concat site-lisp "nxml-mode-20041004/rng-auto.el"))
;; (add-to-list 'auto-mode-alist 
;; 	     '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|config\\|vbproj\\)\\'" . nxml-mode))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

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

(setq mo-nxml-imenu-generic-expression '((nil "<\\(\\w+\\)>" 1))) ;; index elements for imenu

;; (add-to-list 'auto-mode-alist 
;; 	     '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|config\\|vbproj\\)\\'" . nxml-mode))

(add-hook 'nxml-mode-hook 
 	  '(lambda () 
	    (setq nxml-slash-auto-complete-flag t)
	    (define-key nxml-mode-map (kbd "M-h") 'mo-nxml-mark-node)
	    (define-key nxml-mode-map (kbd "C-c i") 'mo-nxml-indent-buffer)
	    (setq imenu-generic-expression mo-nxml-imenu-generic-expression)
	    (linum-mode t)))

;; --------------------------------------------
;; SASS

(load (concat mo-vendor-dir "/sass-mode.el"))
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass\\|\\.scss" . sass-mode))

;; --------------------------------------------
;; YAML

(load (concat mo-vendor-dir "/yaml-mode/yaml-mode.el"))
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; --------------------------------------------
;; Zen Coding mode

(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)