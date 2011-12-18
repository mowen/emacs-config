(push "/usr/local/bin" exec-path)

;; We need to set PATH correctly, as it isn't picked up by Emacs on OS X
;; This solution still isn't quite right.
(setenv "PATH" (shell-command-to-string "source $HOME/.bash_profile && printf $PATH"))

(setq org-directory (expand-file-name "~/Documents/org-files/"))

;; Bind Alt-3 to #
(global-set-key (kbd "M-3")
		(lambda ()
		  (interactive)
		  (insert "#")))

(set-frame-font "Menlo-12")

;; Use the menu-bar-mode on OS X, as it doesn't use any space
(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))

;; Maximise the frame (settings are for my MacBook)
(setq initial-frame-alist
      `((left . 0) (top . 0)
        (width . 180) (height . 53)))
