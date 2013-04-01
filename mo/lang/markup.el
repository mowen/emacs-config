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

(add-hook 'scss-mode-hook
          '(lambda ()
            (esk-paredit-nonlisp)
            (esk-prog-mode-hook)))

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

;; ----------------------------------------
;; web-mode

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun web-mode-hook ()
  "Hooks for Web mode."
  (let ((offset 2))
    (setq web-mode-markup-indent-offset offset)
    (setq web-mode-css-indent-offset offset)
    (setq web-mode-code-indent-offset offset)))

(add-hook 'web-mode-hook  'web-mode-hook)
