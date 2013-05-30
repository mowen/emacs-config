(defun mo-sql-mode-hook ()
  (sql-highlight-ms-keywords)
  (setq imenu-generic-expression
       '((nil "\\s-*procedure[ \n\t]+?\\([a-z0-9_]+\\)" 1))))

(add-hook 'sql-mode-hook 'mo-sql-mode-hook)
