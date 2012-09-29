(load-theme 'deeper-blue)

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

(eval-after-load 'erc-match
  '(progn
    (set-face-foreground 'erc-pal-face "black")
    (set-face-background 'erc-pal-face "yellow1")))

;; ----------------------------------------
;; Alpha (transparency)

(require 'alpha)

(if (string-equal system-type "darwin")
    (transparency-set-value 85)
    (transparency-set-value 95))

