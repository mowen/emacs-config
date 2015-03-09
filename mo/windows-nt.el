(setq magit-git-executable "c:\\program files (x86)\\git\\bin\\git.exe")
(setq magit-emacsclient-executable "\"C:\\ProgramData\\chocolatey\\lib\\Emacs.24.4.0.20141122\\tools\\bin\\emacsclient.exe\"")

(if (eq mo-location 'work)
    (setq org-directory "U:/Documents/OrgMode/")
    (setq org-directory (expand-file-name "~/OrgMode/")))

(setq mo-default-font "Consolas-10.0"
      mo-variable-pitch-font "Segoe UI-10.0"
      mo-fixed-pitch-font "Consolas-10.0")

(setq mo-homepage-dir "C:\\Users\\Martin\\Code\\martinowen.net")

(setq ispell-dictionary "british"
      ispell-program-name "C:\\Program Files (x86)\\aspell\\bin\\aspell.exe")

