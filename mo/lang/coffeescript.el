;;(require 'coffee-mode)

(eval-after-load 'coffee-mode
  '(progn
    (add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
    (add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
    (add-hook 'coffee-mode-hook 'esk-prog-mode-hook)))
