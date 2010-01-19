;; ----------------------------------------
;; Steve Yegge's JS2 Mode for JavaScript
;; ----------------------------------------

(defun mo-js2-comment-function ()
  "Comment the current function."
  (interactive)
  (save-excursion
    (js2-mark-defun)
    (comment-region (region-beginning) (region-end))))

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook 
	  '(lambda ()
	     (setq js2-bounce-indent-flag nil) ;; Don't do the stupid thing where it cycles through indentations.
	     (capitalized-words-mode)
	     (linum-mode)))
