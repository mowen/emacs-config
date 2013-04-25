;; ----------------------------------------
;; Dired Mode

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
