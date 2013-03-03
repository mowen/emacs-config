(mo-log "loading martin.el")

;; This was always called too late when I put it in mo/lang/ruby.el
(remove-hook 'ruby-mode-hook 'inf-ruby-keys) ;; Obsolete hook created by ESK
