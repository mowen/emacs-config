;; ----------------------------------------
;; Ruby Mode
;; ----------------------------------------

(eval-after-load 'starter-kit-ruby-autoloads
  '(progn
    ;; Obsolete hook created by ESK
    (mo-log "remove inf-ruby-keys hook")
    (remove-hook 'ruby-mode-hook 'inf-ruby-keys)))

(eval-after-load 'ruby-mode
  '(progn
    (require 'ruby-block)
    (require 'ruby-electric)
    (require 'inf-ruby)

    (add-hook 'ruby-mode-hook
     '(lambda ()
       (ruby-block-mode)
       (setq ruby-block-highlight-toggle t)
       (local-set-key [f1] 'yari)
       (esk-prog-mode-hook)))

    (add-hook 'ruby-mode-hook 'inf-ruby-setup-keybindings)

    (add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))

    (mo-log "ruby-mode customisations loaded")))  
