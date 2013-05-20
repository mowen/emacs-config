;; I don't need any of this fancy GUI nonsense
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(blink-cursor-mode nil)         ;; No blinking cursor
(column-number-mode t)          ;; Column Numbers in Status Bar
;;(cua-mode t)                  ;; Standard keys for Cut, Copy, Paste, and Undo
;; There is also a shift region thing that I should think about trying

(setq inhibit-startup-screen t)
(setq transient-mark-mode t)       ;; Transient mark = highlight selection.
(setq make-backup-files nil)       ;; Don't leave "blah~" files everywhere.
(setq windmove-wrap-around t)      ;; Wrap around when switching windows with windmove
(setq delete-by-moving-to-trash t) ;; Use the system's trash can when deleting
(setq-default indent-tabs-mode nil) ;; Indentation can't insert tabs
(setq-default tab-width 2)
(setq indent-line-function 'insert-tab)
(setq-default fill-column 80)      ;; Wrap lines in auto-fill mode at 80 chars
(setq-default truncate-lines t)    ;; Seems to work, truncating lines in every buffer.
(savehist-mode 1)                  ;; Save Minibuffer history
(show-paren-mode t)                ;; Show matching parentheses.
(delete-selection-mode +1)         ;; Overwrite if typing while text is selected
(global-auto-revert-mode 1)        ;; Auto-revert buffers if their files have changed
(fset 'yes-or-no-p 'y-or-n-p)      ;; Accept "y" and "n" for confirmations.

;; Enable some things that are turned off by default
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

(setq max-lisp-eval-depth 40000)
(setq max-specpdl-size 100000)

(defalias 'rps 'replace-string)
(defalias 'qr 'query-replace)
(defalias 'cr 'comment-region)
(defalias 'ur 'uncomment-region)
(defalias 'ir 'indent-region)

;; ----------------------------------------
;; Flyspell Mode

(flyspell-mode-off)

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
(add-hook 'midnight-hook 'clean-buffer-list)

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
    ))

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

(eval-after-load 'sr-speedbar
  '(progn
    ;; Highlight the current line
    (add-hook 'speedbar-mode-hook '(lambda () (hl-line-mode 1)))

    (defadvice speedbar-edit-line
     (after mo-speedbar-edit-line-and-other-window)
     "Move to other window after selecting line"
     ;; There's got to be a better way to do this, surely?
     (other-window 1))

    (ad-activate 'speedbar-edit-line)

    (defun mo-toggle-sr-speedbar ()
      "Toggle sr-speedbar buffer, creating it if necessary."
      (interactive)
      (cond ((sr-speedbar-window-p) (other-window 1))
            ((sr-speedbar-exist-p) (sr-speedbar-select-window))
            (t (sr-speedbar-open))))

    (global-set-key (kbd "C-c s") 'mo-toggle-sr-speedbar)
    (global-set-key (kbd "C-c C-s") 'sr-speedbar-close)))

;; ----------------------------------------
;; Expand Region

(require 'expand-region)

(eval-after-load 'expand-region
  '(progn
    (global-set-key (kbd "C-=") 'er/expand-region)))

;; ----------------------------------------
;; Multiple Cursors

(require 'multiple-cursors)

(eval-after-load 'multiple-cursors
  '(progn
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C->") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)))

;; ----------------------------------------
;; Visible Bookmarks

(require 'bm)

(eval-after-load 'bm
  '(progn
    (autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
    (autoload 'bm-next     "bm" "Goto bookmark."                     t)
    (autoload 'bm-previous "bm" "Goto previous bookmark."            t)

    (global-set-key (kbd "<C-f2>") 'bm-toggle)
    (global-set-key (kbd "<f2>")   'bm-next)
    (global-set-key (kbd "<S-f2>") 'bm-previous)))

;; ----------------------------------------
;; yasnippets

(eval-after-load 'yasnippet-bundle
  '(progn
    (yas/load-directory "~/.emacs.d/snippets")))

;; ----------------------------------------
;; Diminish (diminish modeline clutter)

(require 'diminish)

(eval-after-load 'diminish
  '(progn
    (diminish 'undo-tree-mode)
    (diminish 'yas/minor-mode)))

;; ----------------------------------------
;; Emacs Starter Kit

(eval-after-load 'starter-kit-misc
  '(progn
    (remove-hook 'text-mode-hook 'turn-on-auto-fill)
    (remove-hook 'text-mode-hook 'turn-on-flyspell)))

;; ----------------------------------------
;; ANSI Color for shell

(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)

;; ----------------------------------------
;; ido-vertical

(require 'ido-vertical-mode)
(turn-on-ido-vertical)

;; ----------------------------------------
;; Spelling

(setq ispell-personal-dictionary (concat mo-dotfiles-dir ".aspell.en.pws"))

;; ----------------------------------------
;; Workgroups

;; Somebody recommended save-visited-files mode, saying it is better desktop.el
;; Install the package and load like so:
;;
;; (require 'save-visited-files)
;; (turn-on-save-visited-files-mode)

(setq wg-prefix-key (kbd "C-z")
      wg-no-confirm t
      wg-file (concat mo-dotfiles-dir ".workgroups")
      wg-use-faces nil
      wg-switch-on-load t
      wg-morph-on nil)

(defun wg-load-default ()
  "Run `wg-load' on `wg-file'."
  (interactive)
  (if (file-exists-p wg-file)
      (wg-load wg-file)))

(defun wg-save-default ()
  "Run `wg-save' on `wg-file'."
  (interactive)
  (when wg-list
    (with-temp-message ""
      (wg-save wg-file))))

(require 'workgroups)
(eval-after-load 'workgroups
  '(progn
    ;; Useful mappings to revert back to the default workgroups
    ;;(define-key wg-map (kbd "C-l") 'wg-load-default)
    ;;(define-key wg-map (kbd "C-s") 'wg-save-default)
    (workgroups-mode 1)
    (add-hook 'emacs-startup-hook 'wg-load-default)
    (add-hook 'auto-save-hook 'wg-save-default)
    (add-hook 'kill-emacs-hook 'wg-save-default)))

;; ----------------------------------------
;; Browse Kill Ring

(require 'browse-kill-ring)
(eval-after-load 'browse-kill-ring
  '(progn
    (browse-kill-ring-default-keybindings)))
