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

;; -------------------------------------
;; node-repl via js-comint and Node
;; -------------------------------------

(if (string= (symbol-name system-type) "darwin")
    (progn
      (load (concat mo-vendor-dir "/js-comint"))
      (require 'js-comint)
      (setq inferior-js-program-command "node-repl")))
