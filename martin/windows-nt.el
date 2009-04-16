(if (eq mo-location 'work)
    (set-face-attribute 'default nil :height 90 :width 'normal :family "Consolas")
  (set-default-font
   "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1"))

(setq org-directory (expand-file-name "~/OrgMode/"))
