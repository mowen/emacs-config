;; ----------------------------------------
;; ERC (Emacs IRC mode)
;; ----------------------------------------

(require 'erc)
(require 'znc)

(setq erc-interpret-mirc-color t) 	;; Interpret mIRC-style color commands
(setq erc-kill-buffer-on-part t)	;; Kill the buffer when leaving
(setq erc-kill-server-buffer-on-quit t) ;; Kill buffers for server messages after quitting the server

(setq erc-hide-list '("JOIN" "PART" "QUIT")
      erc-pals '("magnars" "technomancy")
      erc-pal-highlight-type 'all)

(defun mo-erc-connect-to-freenode ()
  "Connect to Freenode with ERC."
  (interactive)
  (erc :server "irc.freenode.net"
       :port 6667
       :nick "martyziff"
       :full-name "Martin Owen"
       :password erc-freenode-password))

(defun mo-erc-connect-to-quakenet ()
  "Connect to Quakenet IRC"
  (interactive)
  (erc :server "uk.quakenet.org"
       :port 6667
       :nick "martyziff"
       :full-name "Martin Owen"))

(defun mo-erc-backward-line ()
  "Move backward to previous comment."
  (interactive)
  (re-search-backward "<.*>")
  (beginning-of-line))

(defun mo-erc-forward-line ()
  "Move forward to next comment."
  (interactive)
  (forward-char) ;; move forward a character, or we'll match the name at point
  (re-search-forward "<.*>")
  (beginning-of-line))

(add-hook 'erc-mode-hook
          '(lambda ()
            (local-set-key (kbd "M-p") 'mo-erc-backward-line)
            (local-set-key (kbd "M-n") 'mo-erc-forward-line)))

;; An example of an ERC command:
;; (defun erc-cmd-SAVE (&rest ignore)
;;   "Save your soul!"
;;   (erc-send-message "i'm using the ONE TRUE EDITOR!!!"))

;; (setq erc-autojoin-channels-alist
;;       '(("freenode.net" "##javascript" "#emacs" "#node.js")))

