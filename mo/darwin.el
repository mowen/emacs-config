;; This sets $MANPATH, $PATH and exec-path
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (mapc 'exec-path-from-shell-copy-env '("MY_RUBY_HOME" "RUBYOPT" "RUBY_VERSION" "GEM_HOME" "GEM_PATH" "NODE_PATH" "PYTHONPATH")))

(setq org-directory (expand-file-name "~/Documents/org-files/"))

;; Bind Alt-3 to #
(global-set-key (kbd "M-3")
		(lambda ()
		  (interactive)
		  (insert "#")))

;; Use the command key for meta
(setq ns-command-modifier 'meta)

(set-frame-font "Menlo-12")

;; Use the menu-bar-mode on OS X, as it doesn't use any space
(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))

;; Maximise the frame (settings are for my MacBook)
(setq initial-frame-alist
      `((left . 0) (top . 0)
        (width . 180) (height . 53)))

;; Only load el-get on OS X, as I haven't installed Cygwin on Windows yet.
(load "mo/el-get")   ;; Load el-get packages (depends on auto-complete)
