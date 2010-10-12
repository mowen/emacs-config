;; Martin Owen's Emacs Configuration
;; The "mo-" prefix stands for Martin Owen
;; Originally inspired by http://github.com/defunkt/emacs/tree/master

(require 'cl) ;; Common Lisp

(defvar mo-location 'work
  "Where am I? At \"work\" or at \"home\"?")

(setq mo-dotfiles-dir (file-name-directory load-file-name))
(setq mo-vendor-dir (concat mo-dotfiles-dir "vendor"))

(add-to-list 'load-path mo-dotfiles-dir)
(add-to-list 'load-path mo-vendor-dir)

;; Load passwords.el file if it exists.
(if (file-exists-p (concat mo-dotfiles-dir "martin/passwords.el"))
    (load "martin/passwords"))

;; Load OS specific file
(load (concat "martin/" (symbol-name system-type)))

;; Load ELPA installer
(when (load "vendor/package.el/package.el")
  (package-initialize))

; custom place to save customizations
(setq custom-file "martin/custom.el")
(load custom-file)

(load "martin/util") 	 ;; Utility functions
(load "martin/global")   ;; Global Variables
(load "martin/bindings") ;; Key bindings
(load "martin/theme")    ;; Color Theme
(load "martin/vc")	 ;; Version Control
(load "martin/modes")	 ;; Major Modes
(load "martin/skeletons")

;; Load location specific file
(load (concat "martin/" (symbol-name mo-location)))
