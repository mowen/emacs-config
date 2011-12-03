;; ----------------------------------------
;; Ruby Mode
;; ----------------------------------------

(eval-after-load 'ruby-mode
  '(progn
    (require 'ruby-block)
    (require 'ruby-electric)))

(add-hook 'ruby-mode-hook 
	  '(lambda ()
            (ruby-block-mode t)
            (setq ruby-block-highlight-toggle t)
            (ruby-electric-mode t)
            (esk-prog-mode-hook)))
