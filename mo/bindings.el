;; ----------------------------------------
;; My key bindings

;; Undefined keys (at least in Emacs-Lisp mode)
;; C-x SPC
;; C-'
;; C-.
;; C-#

;; Keys I don't use
;; C-x m (Compose Mail)
;; C-m (Newline)

;; Redefining standard keys
(global-set-key (kbd "C-x C-b") 'ibuffer)	;; Use iBuffer for Buffer List

;; Alternative keys for Alt-X
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-c C-m") 'smex)

(global-set-key [C-tab] 'other-window)
(global-set-key (kbd "C-z") 'undo) ;; Fix the stupidest default keybinding in history

(global-set-key (kbd "C-x g") 'rename-buffer)
(global-set-key (kbd "C-x C-g") 'keyboard-quit)

(global-set-key (kbd "M-k")   'mo-copy-line-as-kill)  ; Used to be kill-sentence	
(global-set-key [M-backspace] 'mo-copy-word-as-kill)  ; Used to be backward-kill-word

(global-set-key (kbd "C-x t") 'mo-create-temp-buffer)
(global-set-key (kbd "C-x c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x p") '(lambda (&optional arg)
                                (interactive "p")
                                (cond ((= 1 arg) (el-get-list-packages))
                                      ((= 2 arg) (package-list-packages))
                                      (t (package-list-packages-no-fetch)))))

;; -----------------------------------------------------------------------------
;; Function keys

(global-set-key [f5] 'eshell)
(global-set-key [f7] 'mo-erc-connect-to-freenode)

;; -----------------------------------------------------------------------------
;; Org Mode Keys

(global-set-key (kbd "C-c r") 'org-remember)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; -----------------------------------------------------------------------------
;; Window switching

(global-set-key [M-left]  'windmove-left)  ; move to left windnow
(global-set-key [M-right] 'windmove-right) ; move to right window
(global-set-key [M-up]    'windmove-up)    ; move to upper window
(global-set-key [M-down]  'windmove-down)  ; move to downer window
