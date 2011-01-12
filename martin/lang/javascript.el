;; ----------------------------------------
;; Espresso Mode for JavaScript
;; ----------------------------------------

(setq mo-js-imenu-generic-expression
      '((nil "function\\s-+\\(\\w+\\)\\s-*(" 1) ;; Named Function
        (nil "^\\s-*\\(_?\\w+\\):\\s-*function\\s-*(" 1) ;; Hash Method
        (nil "this\.\\(\\w+\\)\\s-*=\\s-*function\\s-*(" 1) ;; Instance Method
	(nil "var \\([A-Z]+\\w+\\) = {" 1))) ;; Variable as Class
 
(autoload #'espresso-mode "espresso" "Start espresso-mode" t)
(add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
(add-to-list 'auto-mode-alist '("\\.json$\\|\\.json.get$" . espresso-mode))

(add-hook 'espresso-mode-hook
	  '(lambda ()
	     (setq imenu-create-index-function 'imenu-default-create-index-function)
	     (setq imenu-generic-expression mo-js-imenu-generic-expression)
	     (capitalized-words-mode t)
	     (linum-mode t)))

;; -------------------------------------
;; node-repl via js-comint and Node
;; -------------------------------------

(if (string= (symbol-name system-type) "darwin")
    (progn
      (load (concat mo-vendor-dir "/js-comint"))
      (require 'js-comint)
      (setq inferior-js-program-command "node-repl")))
