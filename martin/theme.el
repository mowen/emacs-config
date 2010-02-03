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
     (cond
      ((eq mo-location 'work)
       (color-theme-vim-colors))
      (t
       (load (concat mo-vendor-dir "/zenburn.el"))
       (zenburn)))))

;; ----------------------------------------
;; My custom faces
;;
;; - list-colors-display gets the available colours
;; - list-faces-display gets the current faces.
;;
 
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-background 'diff-added "honeydew")
     (set-face-foreground 'diff-removed "red3")
     (set-face-background 'diff-removed "misty rose")))
 
(eval-after-load 'magit
  '(progn
     (set-face-foreground 'magit-diff-add "green3")
     (set-face-foreground 'magit-diff-del "red3")))