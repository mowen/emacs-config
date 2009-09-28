;; ----------------------------------------
;; Color Themes
;; ----------------------------------------

;; Good color-themes:
;; - arjen
;; - jsc-light2
;; - andreas (light)
;; - clarity (a good dark one)
;; - vim-colors (light)

(add-to-list 'load-path (concat mo-vendor-dir "/color-theme/"))

(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (if (eq mo-location 'work)
	 (color-theme-vim-colors) 
       ;;(load (concat mo-vendor-dir "/color-theme-twilight"))
       ;;(color-theme-twilight)
       (load (concat mo-vendor-dir "/zenburn.el"))
       (zenburn))))


