;; ----------------------------------------
;; ERC (Emacs IRC mode)
;; ----------------------------------------

(require 'erc)

(setq erc-interpret-mirc-color t) 	;; Interpret mIRC-style color commands
(setq erc-kill-buffer-on-part t)	;; Kill the buffer when leaving
(global-set-key "\C-cef"
                ;; Connect to the Freenode via ERC.
                (lambda () 
                  (interactive)
                  (erc :server "irc.freenode.net" 
                       :port "6667" 
                       :nick "martyziff"
                       :full-name "Martin Owen")))
;;(setq erc-autojoin-channels-alist '(("freenode.net" "#geekup"
;;                                                    "#emacs")))
