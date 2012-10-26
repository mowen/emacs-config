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
(setq windmove-wrap-around t)	   ;; Wrap around when switching windows with
;; windmove
(setq delete-by-moving-to-trash t) ;; Use the system's trash can when deleting
(setq-default fill-column 80)	   ;; Wrap lines in auto-fill mode at 80 chars
(setq-default truncate-lines t)	   ;; Seems to work, truncating lines in every
;; buffer.
(savehist-mode 1)		   ;; Save Minibuffer history
(show-paren-mode t)		   ;; Show matching parentheses.
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

;; ----------------------------------------
;; Midnight Mode

(require 'midnight)
(midnight-delay-set 'midnight-delay 0) ;; Run midnight hook 0 seconds after midnight
(add-hook 'midnight-hook '(lambda ()
                           (clean-buffer-list)))

;; ----------------------------------------
;; Auto Complete

(require 'auto-complete)
(eval-after-load 'auto-complete
  '(progn
    ;;(add-to-list 'ac-dictionary-directories (concat mo-dotfiles-dir "elpa/auto-complete-1.4/dict"))  
    (require 'auto-complete-config)

    (ac-config-default)
    (ac-flyspell-workaround)
    (ac-linum-workaround)

    (setq-default global-auto-complete-mode t)
    (setq-default ac-candidate-limit 200) ;; ac-dabbrev breaks if this is left as nil

    (load (concat mo-vendor-dir "/ac-dabbrev"))
    (require 'ac-dabbrev)
    (setq-default ac-sources (append ac-sources '(ac-source-dabbrev)))))

;; ----------------------------------------
;; Linkd (links in text files)

(load (concat mo-vendor-dir "/linkd"))
(require 'linkd)

;; ----------------------------------------
;; Bookmark+

(require 'bookmark+)

;; ----------------------------------------
;; Speedbar

(require 'sr-speedbar)

(setq speedbar-hide-button-brackets-flag t
      speedbar-show-unknown-files t
      speedbar-smart-directory-expand-flag t
      speedbar-use-images nil
      speedbar-indentation-width 2
      speedbar-update-flag t
      sr-speedbar-width 30
      sr-speedbar-width-x 30
      sr-speedbar-auto-refresh t
      sr-speedbar-skip-other-window-p t
      sr-speedbar-right-side nil)

;; More familiar keymap settings.
(add-hook 'speedbar-reconfigure-keymaps-hook
          '(lambda ()
             (define-key speedbar-mode-map (kbd "^") 'speedbar-up-directory) 
             (define-key speedbar-mode-map [S-up] 'speedbar-up-directory)
             (define-key speedbar-mode-map [right] 'speedbar-flush-expand-line)
             (define-key speedbar-mode-map [left] 'speedbar-contract-line)))

;; Highlight the current line
(add-hook 'speedbar-mode-hook '(lambda () (hl-line-mode 1)))

;; (defadvice speedbar-edit-line
;;     (after mo-speedbar-edit-line-and-other-window)
;;   "Move to other window after selecting line"
;;   (message "Using my Other Window")
;;   (other-window 1))

;; (ad-activate 'speedbar-edit-line)

(defun mo-toggle-sr-speedbar ()
  "Toggle sr-speedbar buffer, creating it if necessary."
  (interactive)
  (cond ((sr-speedbar-window-p) (other-window 1))
        ((sr-speedbar-exist-p) (sr-speedbar-select-window))
        (t (sr-speedbar-open))))
