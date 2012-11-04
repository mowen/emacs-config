(setq magit-git-executable "c:/program files (x86)/git/bin/git.exe")

(if (eq mo-location 'work)
    (setq org-directory "U:/Documents/OrgMode/")
    (setq org-directory (expand-file-name "~/OrgMode/")))

(setq mo-default-font "Consolas-10")
(setq mo-variable-pitch-font "Segoe UI-10")
(setq mo-fixed-pitch-font "Consolas-10")

(custom-set-variables
    '(ispell-dictionary "british")
    '(ispell-program-name "C:\\Program Files (x86)\\aspell\\bin\\aspell.exe"))
