(push "/usr/local/bin" exec-path)

(setq org-directory (expand-file-name "~/Documents/org-files/"))

;; Bind Alt-3 to #
(global-set-key (kbd "M-3")
		(lambda ()
		  (interactive)
		  (insert "#")))

(set-frame-font "Menlo-12")

;; Maximise the frame (settings are for my MacBook)
(setq initial-frame-alist
      `((left . 0) (top . 0)
        (width . 180) (height . 53)))
