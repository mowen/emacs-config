;; ----------------------------------------
;; ERC (Emacs IRC mode)
;; ----------------------------------------

(require 'erc)

(require 'erc-highlight-nicknames)
(add-to-list 'erc-modules 'highlight-nicknames)
(erc-update-modules)

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

;; An example of an ERC command:
;; (defun erc-cmd-SAVE (&rest ignore)
;;   "Save your soul!"
;;   (erc-send-message "i'm using the ONE TRUE EDITOR!!!"))

;; (setq erc-autojoin-channels-alist
;;       '(("freenode.net" "##javascript" "#emacs" "#node.js")))
