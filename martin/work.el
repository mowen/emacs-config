(load "martin/work/skeletons")
(load "martin/work/util")

(global-hl-line-mode -1) ;; Don't highlight the current line

(define-project-type qore-ui (generic)
  (look-for "Qire.Qore.UI")
  :relevant-files ("\.js$" "\.ejs$" "\.html$")
  :irrelevant-files ("jquery/" "steal/" "funcunit/")
  :project-name (lambda (root) "Qire.Qore.UI"))
  
