;; ----------------------------------------
;; Ruby Mode Customisations
;; ----------------------------------------

(add-to-list 'load-path (concat mo-vendor-dir "/ruby"))

(require 'ruby-electric)
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.\\(rb\\|gemspec\\|rake\\)$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(Rakefile\\|Sitefile\\)" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook 
	  '(lambda () 
	     (inf-ruby-keys)
	     (ruby-electric-mode t)
	     (linum-mode)
	     ;;(pabbrev-mode t)
	     (define-key ruby-mode-map "\C-c\C-a" 'ruby-eval-buffer)))

(defun ruby-eval-buffer () (interactive)
  "Evaluate the buffer with ruby."
  (shell-command-on-region (point-min) (point-max) "ruby"))

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
;;(add-to-list auto-mode-alist '("\\.rb\\'" . rhtml-mode))
