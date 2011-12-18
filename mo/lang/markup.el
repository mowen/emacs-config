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
;; SASS/SCSS

(require 'scss-mode)
(setq scss-compile-at-save nil)

;; --------------------------------------------
;; Zen Coding mode

(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode)

;; ----------------------------------------
;; YAML Mode
;; ----------------------------------------

(autoload 'yaml-mode "yaml-mode" "\
Major mode for editing Yaml files.

\\{yaml-mode-map}

\(fn)" t nil)

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
