(defvar mo-location (if (string-match "TERIYAKI" (system-name)) 'work 'home)
  "Where am I? At \"work\" or at \"home\"?")

(setq mo-dotfiles-dir (file-name-directory load-file-name))
(setq mo-vendor-dir (concat mo-dotfiles-dir "vendor"))

(add-to-list 'load-path mo-dotfiles-dir)
(add-to-list 'load-path mo-vendor-dir)

(defun mo-load (file-symbol)
  (load (format "mo/%s" (symbol-name file-symbol))))

(mo-load 'logger)

;; ----------------------------------------
;; Packages

(require 'package)
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar mo-packages '(ace-jump-mode
                      ac-dabbrev
                      ac-nrepl
                      ac-slime
                      alert
                      alpha
                      all
                      auto-complete
                      bookmark+
                      browse-kill-ring
                      bm
                      buffer-move
                      clojure-mode
                      clojure-test-mode
                      coffee-mode
                      color-theme
                      color-theme-sanityinc-tomorrow
                      csharp-mode
                      dash
                      diminish
                      dired-details
                      durendal
                      erc-hl-nicks
                      el-mock
                      exec-path-from-shell
                      expand-region
                      gist
                      haml-mode
                      ht
                      js2-mode
                      ido-vertical-mode
                      ioccur
                      magit
                      magithub
                      markdown-mode
                      marmalade
                      multiple-cursors
                      notify
                      nrepl
                      org
                      rinari
                      ruby-block
                      ruby-electric
                      ruby-mode
                      ruby-test-mode
                      s ;; string manipulation lib
                      scss-mode
                      skewer-mode
                      slime
                      slime-repl
                      sr-speedbar
                      starter-kit
                      starter-kit-bindings
                      starter-kit-eshell
                      starter-kit-js
                      starter-kit-lisp
                      starter-kit-ruby
                      undo-tree
                      web-mode
                      workgroups
                      yaml-mode
                      yari
                      yasnippet-bundle))

(dolist (p mo-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; ----------------------------------------
;; Everything else

;; Load passwords.el file if it exists.
(if (file-exists-p (concat mo-dotfiles-dir "mo/passwords.el"))
    (progn
      (mo-log "loading mo/passwords.el")
      (mo-load 'passwords)))

(mo-load system-type)
(mo-load mo-location)

;; custom place to save customisations
(setq custom-file (concat mo-dotfiles-dir "mo/custom.el"))
(load custom-file)

(mapc 'mo-load '(util bindings hp global modes theme))
