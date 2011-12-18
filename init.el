(defvar mo-location (if (string-match "QIRE" (system-name)) 'work 'home)
  "Where am I? At \"work\" or at \"home\"?")

(setq mo-dotfiles-dir (file-name-directory load-file-name))
(setq mo-vendor-dir (concat mo-dotfiles-dir "vendor"))

(add-to-list 'load-path mo-dotfiles-dir)
(add-to-list 'load-path mo-vendor-dir)

;; ----------------------------------------
;; Packages

(require 'package)
(add-to-list 'package-archives '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar mo-packages '(ace-jump-mode
                      clojure-mode
                      clojure-test-mode
                      coffee-mode
                      durendal
                      erc-hl-nicks
                      gist
                      haml-mode
		      magit
                      magithub
                      markdown-mode
		      marmalade
                      org
                      ruby-mode
                      ruby-electric
                      ruby-block
                      scss-mode
                      slime
                      slime-repl
                      starter-kit
                      starter-kit-bindings
                      starter-kit-eshell
                      starter-kit-js
                      starter-kit-lisp
                      starter-kit-ruby
                      undo-tree
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
    (load "mo/passwords"))

(load (concat "mo/" (symbol-name system-type)))
(load (concat "mo/" (symbol-name mo-location)))

;; custom place to save customizations
(setq custom-file "mo/custom.el")
(load custom-file)

(load "mo/el-get")   ;; Load el-get packages
(load "mo/util")     ;; Utility functions
(load "mo/global")   ;; Global Variables
(load "mo/bindings") ;; Key bindings
(load "mo/theme")    ;; Color Theme
(load "mo/modes")    ;; Major Modes
