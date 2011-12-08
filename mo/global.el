;; I don't need any of this fancy GUI nonsense
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(blink-cursor-mode nil)		;; No blinking cursor
(column-number-mode t)		;; Column Numbers in Status Bar
;;(cua-mode t) 			;; Standard keys for Cut, Copy, Paste, and Undo
;; There is also a shift region thing that I should think about trying

(setq inhibit-startup-screen t)
(setq transient-mark-mode t)	   ;; Transient mark = highlight selection.
(setq make-backup-files nil)	   ;; Don't leave "blah~" files everywhere.
(setq windmove-wrap-around t)	   ;; Wrap around when switching windows with windmove
(setq-default fill-column 80)	   ;; Wrap lines in auto-fill mode at 80 chars
(setq-default truncate-lines t)	   ;; Seems to work, truncating lines in every buffer.
(savehist-mode 1)		   ;; Save Minibuffer history
(show-paren-mode t)		   ;; Show matching parentheses.
(global-hl-line-mode 1)            ;; Highlight the current line
(global-auto-revert-mode 1)        ;; Auto-revert buffers if their files have changed
(fset 'yes-or-no-p 'y-or-n-p)	   ;; Accept "y" and "n" for confirmations.
(put 'upcase-region 'disabled nil)   ;; Don't disable Upper Case Region command
(put 'downcase-region 'disabled nil) ;; Or Down Case Region

(setq max-lisp-eval-depth 40000)
(setq max-specpdl-size 100000)

(defalias 'rps 'replace-string)
(defalias 'qr 'query-replace)
(defalias 'cr 'comment-region)
(defalias 'ur 'uncomment-region)
(defalias 'ir 'indent-region)

;; ----------------------------------------
;; Bookmarks

;; When opening a bookmark, name the buffer after the bookmark
;; TODO: maybe use uniquify to generate the buffer name? bookmark
;; name is probably easier to remember
(add-hook 'bookmark-after-jump-hook
	  '(lambda ()
	     (rename-buffer bookmark-current-bookmark)))

;; ----------------------------------------
;; Desktop mode (save state when exiting)

(setq desktop-dirname (expand-file-name "~")) ;; Where to save .emacs.desktop
(desktop-save-mode 1)

;; ----------------------------------------
;; Undo Tree Mode

(require 'undo-tree)
(global-undo-tree-mode 1)

;; ----------------------------------------
;; Ace Jump Mode

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

