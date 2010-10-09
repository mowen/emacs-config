;; ----------------------------------------
;; ERC (Emacs IRC mode)
;; ----------------------------------------

(require 'erc)

(setq erc-interpret-mirc-color t) 	;; Interpret mIRC-style color commands
(setq erc-kill-buffer-on-part t)	;; Kill the buffer when leaving

(defun mo-erc-connect-to-freenode ()
  "Connect to Freenode with ERC."
  (interactive)
  (erc :server "irc.freenode.net"
       :port 6667
       :nick "martyziff"
       :full-name "Martin Owen"
       :password erc-freenode-password))

;; (setq erc-autojoin-channels-alist
;;       '(("freenode.net" "#mongodb" "#emacs" "#node.js")))
