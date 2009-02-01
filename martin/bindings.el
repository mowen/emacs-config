;; ----------------------------------------
;; My key bindings

;; Undefined keys (at least in Emacs-Lisp mode)
;; C-x t
;; C-x g
;; C-x x
;; C-,
;; C-'
;; C-.
;; C-#

;; Keys I don't use
;; C-x m (Compose Mail)
;; C-m (Newline)

;; Redefining standard keys
(global-set-key (kbd "C-x C-b") 'ibuffer)	;; Use iBuffer for Buffer List

;; Alternative keys for Alt-X
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command)

(global-set-key (kbd "C-x g") 'rename-buffer)
(global-set-key (kbd "C-x C-g") 'keyboard-quit)

(global-set-key (kbd "M-k") ;; Used to be kill-sentence
		'mo-copy-line-as-kill)
(global-set-key [M-backspace] ;; Used to be backward-kill-word
		'mo-copy-word-as-kill)
(global-set-key [C-backspace] ;; Used to be backward-kill-word
		'mo-backward-kill-word)
;;(global-set-key "\M-q" 'mo-quote-word)

;; -----------------------------------------------------------------------------
;; Org Mode Keys

(global-set-key [f8] 'org-remember)
(global-set-key (kbd "C-c r") 'org-remember)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
