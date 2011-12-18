;; ----------------------------------------
;; Text Mode

(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 
	  '(lambda ()
	    (abbrev-mode)
	    (turn-on-visual-line-mode)
            (auto-complete-mode t)
	    (if (and (buffer-file-name)
		     (string-match "\\.textile$" (buffer-file-name)))
		(progn
		  ;; Turn on Textile Minor Mode for Textile buffers
		  (require 'textile-minor-mode)
		  (textile-minor-mode t)
		  (setq truncate-lines nil)))))

(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
