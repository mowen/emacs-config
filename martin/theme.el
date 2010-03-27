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
     ;; Temporaily set all themes to Vim as Org Mode isn't right with Zenburn
     (color-theme-vim-colors)))

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

(eval-after-load 'org
  '(progn
     (set-face-foreground 'org-level-2 "magenta4")
     (set-face-foreground 'outline-2 "magenta4")
     (set-face-foreground 'org-level-6 "red3")
     (set-face-foreground 'outline-6 "red3")))