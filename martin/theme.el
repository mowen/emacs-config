;; ----------------------------------------
;; Color Themes
;; ----------------------------------------

;; Good color-themes:
;; - arjen
;; - jsc-light2
;; - andreas (light)
;; - clarity (a good dark one)
;; - vim-colors (light)

(require 'color-theme)
(color-theme-initialize)

;;(color-theme-vim-colors)

;;(load (concat mo-vendor-dir "/color-theme-twilight"))
;;(color-theme-twilight)

(load (concat mo-vendor-dir "/zenburn.el"))
(zenburn)