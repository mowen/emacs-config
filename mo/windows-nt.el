(setq magit-git-executable "c:\\program files (x86)\\git\\bin\\git.exe")
(setq magit-emacsclient-executable "\"c:\\program files (x86)\\emacs-24.3\\bin\\emacsclient.exe\"")

(if (eq mo-location 'work)
    (setq org-directory "U:/Documents/OrgMode/")
    (setq org-directory (expand-file-name "~/OrgMode/")))

(setq mo-default-font "Consolas-10"
      mo-variable-pitch-font "Segoe UI-10"
      mo-fixed-pitch-font "Consolas-10")

(setq mo-homepage-dir "C:\\Users\\Martin\\Code\\martinowen.net")

(setq ispell-dictionary "british"
      ispell-program-name "C:\\Program Files (x86)\\aspell\\bin\\aspell.exe")

