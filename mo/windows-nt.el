(setq magit-git-executable "c:/program files (x86)/git/bin/git.exe")

(if (eq mo-location 'work)
    (setq org-directory "U:/Documents/OrgMode/")
    (setq org-directory (expand-file-name "~/OrgMode/")))

(set-default-font "-outline-Consolas-normal-r-normal-normal-14-97-96-96-c-*-iso8859-1")

(custom-set-variables
    '(ispell-dictionary "british")
    '(ispell-program-name "C:\\Program Files (x86)\\aspell\\bin\\aspell.exe"))
