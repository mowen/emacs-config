;; ----------------------------------------
;; Org Mode
;; ----------------------------------------

(eval-after-load 'org
  '(progn
     ;; org-directory is set in the platform-specific files (darwin, windows-nt)
     (setq org-default-notes-file (concat org-directory "Notes.org"))
     (setq org-archive-location (concat org-directory "Archive.org"))

     (setq org-hide-leading-stars t
           org-startup-folded t
           org-log-done nil)

     ;; HTML Export settings

     (defun mo-css-include (filename)
       "HTML 'link' tag to include the CSS file FILENAME."
       (format "<link rel=\"stylesheet\" type=\"text/css\" href=\"%s\" />" filename))

     (setq org-export-html-style-include-scripts nil
           org-export-html-style-include-default nil)
     (setq org-export-html-style
           (mo-css-include "org-style.css"))

     ;; See "bindings.el" for Org's global key mappings

     (defun org-summary-todo (n-done n-not-done)
       "Switch entry to DONE when all subentries are done, to TODO otherwise."
       (let (org-log-done org-log-states)   ; turn off logging
         (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

     (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

     (setq mo-org-home-dir (if (eq mo-location 'work)
                               "u:/Documents/HomeOrg/" ;; I store my home org files separately when I'm in work.
                             org-directory))

    (add-to-list 'load-path (concat mo-vendor-dir "/org-pomodoro"))
    (require 'org-pomodoro)))

