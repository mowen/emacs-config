;; ----------------------------------------
;; Steve Yegge's JS2 Mode for JavaScript
;; ----------------------------------------

(defun mo-js2-comment-function ()
  "Comment the current function."
  (interactive)
  (save-excursion
    (js2-mark-defun)
    (comment-region (region-beginning) (region-end))))

(setq mo-js-imenu-generic-expression
      '((nil "function\\s-+\\(\\w+\\)\\s-*(" 1) ;; Named Function
        (nil "^\\s-*\\(_?\\w+\\):\\s-*function\\s-*(" 1) ;; Hash Method
        (nil "this\.\\(\\w+\\)\\s-*=\\s-*function\\s-*(" 1) ;; Instance Method
	(nil "var \\([A-Z]+\\w+\\) = {" 1))) ;; Variable as Class
 
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook 
	  '(lambda ()
	     (setq js2-bounce-indent-flag nil) ;; Don't do the stupid thing where it cycles through indentations.
	     (setq imenu-create-index-function 'imenu-default-create-index-function) ;; Don't use the js2 imenu index function
	     (setq imenu-generic-expression mo-js-imenu-generic-expression)
	     (capitalized-words-mode)
	     (linum-mode)))
