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
  (re-search-backward "^<.*>")
  (beginning-of-line))

(defun mo-erc-forward-line ()
  "Move forward to next comment."
  (interactive)
  (forward-char) ;; move forward a character, or we'll match the name at point
  (re-search-forward "^<.*>")
  (beginning-of-line))

(defun mo-erc-backward-pal ()
  "Move backward to the next comment by a pal."
  (interactive)
  (let ((current-line (line-number-at-pos (point))))
    (mo-erc-backward-pal)
    (while (and (not (mo-erc-comment-is-by-pal-p))
                (not (= current-line (line-number-at-pos (point)))))
      (mo-erc-backward-line))))

(defun mo-erc-forward-pal ()
  "Move forward to the next comment by a pal."
  (interactive)
  (let ((current-line (line-number-at-pos (point))))
    (mo-erc-forward-line)
    (while (and (not (mo-erc-comment-is-by-pal-p))
                (not (= current-line (line-number-at-pos (point)))))
      (mo-erc-forward-line))))

(defun mo-erc-comment-is-by-pal-p ()
  "Is the comment under point made by a pal?"
  (save-excursion
    (mo-erc-forward-line) ;; go backwards and forwards to ensure 
    (mo-erc-backward-line)  ;; we are at the start of the name
    (let ((current-face (get-text-property (point) 'face)))
      (equal 'erc-pal-face current-face))))

(add-hook 'erc-mode-hook
          '(lambda ()
            (local-set-key [M-up] 'mo-erc-backward-line)
            (local-set-key [M-down] 'mo-erc-forward-line)
            (local-set-key (kbd "C-x <up>") 'mo-erc-backward-pal)
            (local-set-key (kbd "C-x <down>") 'mo-erc-forward-pal)))

;; An example of an ERC command:
;; (defun erc-cmd-SAVE (&rest ignore)
;;   "Save your soul!"
;;   (erc-send-message "i'm using the ONE TRUE EDITOR!!!"))

;; (setq erc-autojoin-channels-alist
;;       '(("freenode.net" "##javascript" "#emacs" "#node.js")))

