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

(defvar mo-script-url-alist
  '((jquery . "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js")
    (json2 . "http://cdnjs.cloudflare.com/ajax/libs/json2/20110223/json2.js")
    (ocanvas . "http://cdnjs.cloudflare.com/ajax/libs/ocanvas/1.0/ocanvas.min.js")
    (processing . "http://cdnjs.cloudflare.com/ajax/libs/processing.js/1.2.1/processing-api.min.js")
    (prototype . "https://ajax.googleapis.com/ajax/libs/prototype/1.7.0.0/prototype.js")
    (sizzle . "http://cdnjs.cloudflare.com/ajax/libs/sizzle/1.4.4/sizzle.min.js")
    (underscore . "http://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.1.7/underscore-min.js")
    (underscore-string . "http://cdnjs.cloudflare.com/ajax/libs/underscore.string/1.1.4/underscore.string.min.js")
    (waypoints . "http://cdnjs.cloudflare.com/ajax/libs/waypoints/1.1/waypoints.min.js"))
  "URLs for loading different JavaScript libraries.")

(defun mo-get-script-url (library)
  "Get a script URL for the given library."
  (cdr (assoc library mo-script-url-alist)))

(defun mo-script-tag-insert-for-library (library)
  (let ((script-url (mo-get-script-url library)))
    (concat "var script = document.createElement('script'); "
	    "script.type = 'text/javascript'; "
	    "script.src='" script-url "'; "
	    "head.appendChild(script);" )))

(defun mo-moz-repl-enter-content ()
  "Enter the main content in the MozRepl. Necessary for working with the DOM."
  (comint-send-string (inferior-moz-process)
		      (concat moz-repl-name ".enter(content); ")))
    
(defun mo-moz-repl-load-scripts-list (libraries)
  (let ((script-tag-inserts (mapconcat 'mo-script-tag-insert-for-library libraries "")))
    (mo-moz-repl-enter-content) ;; This needs to run first or document isn't set
    (comint-send-string (inferior-moz-process)
			(concat
			 "var head = document.getElementsByTagName('head').item(0); "
			 script-tag-inserts))))

(defun mo-moz-repl-load-scripts (lib-string)
  "Load the given scripts in a MozRepl session."
  (interactive "MScripts to load: ")
  (let* ((libraries (map 'list 'intern (split-string lib-string " "))))
    (mo-moz-repl-load-scripts-list libraries)))

;; The function below doesn't actually work.
(defun mo-moz-repl-open-url (url &optional load-libraries)
  "Open the given URL with MozRepl, and optionally load some JavaScript libraries."
  (mo-moz-repl-enter-content)
  (comint-send-string (inferior-moz-process)
		      (concat "document.location.href = '" url "'; "
			      "undefined;\n"))
  (sleep-for 0 500) ;; 500ms
  (insert "\n")
  (mo-moz-repl-enter-content) ;; Set context again as it has reloaded
  (if load-libraries
      (mo-moz-repl-load-scripts-list load-libraries)))

;; (mo-moz-repl-open-url "http://martinowen.net" '(jquery underscore))


