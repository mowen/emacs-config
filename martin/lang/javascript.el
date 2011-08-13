;; ----------------------------------------
;; Espresso Mode for JavaScript
;; ----------------------------------------

(setq mo-js-imenu-generic-expression
      '((nil "function\\s-+\\(\\w+\\)\\s-*(" 1) ;; Named Function
        (nil "^\\s-*\\(_?\\w+\\):\\s-*function\\s-*(" 1) ;; Hash Method
        (nil "this\.\\(\\w+\\)\\s-*=\\s-*function\\s-*(" 1) ;; Instance Method
	(nil "var \\([A-Z]+\\w+\\) = {" 1))) ;; Variable as Class
 
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json$\\|\\.json.get$" . js-mode))

(add-hook 'js-mode-hook
	  '(lambda ()
	     (setq imenu-create-index-function 'imenu-default-create-index-function)
	     (setq imenu-generic-expression mo-js-imenu-generic-expression)
	     (setq indent-tabs-mode nil)
	     (capitalized-words-mode t)
	     (linum-mode t)))

;; ----------------------------------------
;; jquery-doc.el

(add-to-list 'load-path (concat mo-vendor-dir "/jquery-doc"))
(require 'jquery-doc)
(add-hook 'js-mode-hook 'jquery-doc-setup) ;; adds ac-source-jquery to the ac-sources list

;; ----------------------------------------
;; Coffee Script
;; ----------------------------------------

(load (concat mo-vendor-dir "/coffee-mode"))
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

;; ----------------------------------------
;; Integrate with MozRepl
;; ----------------------------------------

(load (concat mo-vendor-dir "/moz"))
(add-hook 'js-mode-hook '(lambda () (moz-minor-mode 1)))

(defun mo-load-script-in-moz-repl (script-url)
  "Load a script in a MozRepl session."
  (let ((script-tag-insert (concat "var script = document.createElement('script');"
				   "script.type = 'text/javascript';"
				   "script.src='" script-url "';"
				   "var head = document.getElementsByTagName('head').item(0);"
				   "head.appendChild(script);")))
    (insert "repl.enter(content);")
    (comint-send-input)
    (insert script-tag-insert)
    (comint-send-input)))

(defmacro mo-define-script-loading-defuns (script-urls-alist)
  "Define functions for loading the various scripts in a MozRepl session."
  (declare (indent defun))
  `(progn
     ,@(mapcar (lambda (script-url)
		 (list 'defun (intern (format "mo-load-%s-in-moz-repl" (car script-url))) '()
		       (concat "Load " (symbol-name (car script-url)) " in a MozRepl session.")
		       (list 'interactive)
		       (list 'mo-load-script-in-moz-repl (cdr script-url))))
	       script-urls-alist)))

(mo-define-script-loading-defuns
    ((jquery . "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js")
     (json2 . "http://cdnjs.cloudflare.com/ajax/libs/json2/20110223/json2.js")
     (ocanvas . "http://cdnjs.cloudflare.com/ajax/libs/ocanvas/1.0/ocanvas.min.js")
     (processing . "http://cdnjs.cloudflare.com/ajax/libs/processing.js/1.2.1/processing-api.min.js")
     (prototype . "https://ajax.googleapis.com/ajax/libs/prototype/1.7.0.0/prototype.js")
     (sizzle . "http://cdnjs.cloudflare.com/ajax/libs/sizzle/1.4.4/sizzle.min.js")
     (underscore . "http://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.1.7/underscore-min.js")
     (underscore-string . "http://cdnjs.cloudflare.com/ajax/libs/underscore.string/1.1.4/underscore.string.min.js")
     (waypoints . "http://cdnjs.cloudflare.com/ajax/libs/waypoints/1.1/waypoints.min.js")))