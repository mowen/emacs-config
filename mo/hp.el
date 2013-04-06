;; --------------------------------------------------
;; Functions to help with editing martinowen.net

(defun mo-compile-homepage ()
  (interactive)
  (let ((homepage-command-format (if (eq system-type 'darwin)
                                     "cd %s; nanoc co"
                                     "cd %s && nanoc co")))
      (if (boundp 'mo-homepage-dir)
          (compile (format homepage-command-format mo-homepage-dir))
          (message "You need to set the 'mo-homepage-dir' variable."))))

(global-set-key (kbd "C-c h") 'mo-compile-homepage)
