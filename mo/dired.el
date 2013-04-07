;; ----------------------------------------
;; Dired Mode

(add-hook 'dired-mode-hook
	  '(lambda ()
	     (define-key dired-mode-map (kbd "C-c C-s")
	       '(lambda ()
		  (interactive)
		  (mo-context-sensitive-vc-status default-directory)))))

;; Auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; ----------------------------------------
;; Use dired-details

(require 'dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

;; ----------------------------------------
;; TRAMP Setup

(require 'tramp)
(setq tramp-auto-save-directory (expand-file-name "~/tmp"))
(setq tramp-default-method "plink")
