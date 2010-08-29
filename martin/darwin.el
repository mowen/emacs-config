(setq org-directory (expand-file-name "~/Documents/OrgMode/"))

;; Bind Alt-3 to #
(global-set-key (kbd "M-3")
		(lambda ()
		  (interactive)
		  (insert "#")))

;; Maximise the frame (settings are for my MacBook)
(setq initial-frame-alist
      `((left . 0) (top . 0)
        (width . 180) (height . 50)))