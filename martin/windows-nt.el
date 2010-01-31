(if (eq mo-location 'work)
    (progn
      (setq org-directory "H:/Emacs/OrgMode/")
      (set-face-attribute 'default nil :height 90 :width 'normal :family "Consolas"))
  (setq org-directory (expand-file-name "~/OrgMode/"))
  (set-default-font "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1"))
