(if (eq mo-location 'work)
    (setq org-directory "U:/Documents/OrgMode/"
          magit-git-executable "c:/program files (x86)/git/bin/git.exe")
    (setq org-directory (expand-file-name "~/OrgMode/")
          magit-git-executable "c:/program files/git/bin/git.exe"))

(set-default-font "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1")
