;; ----------------------------------------
;; Org Mode
;; ----------------------------------------

(add-to-list 'load-path (concat mo-vendor-dir "/org-mode/lisp"))
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; org-directory is set in the platform-specific files (darwin, windows-nt)
(setq org-default-notes-file (concat org-directory "Notes.org"))
(setq org-archive-location (concat org-directory "Archive.org"))
(org-remember-insinuate)

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

(setq org-remember-templates
      '(("Todo" ?t "* TODO %?\n  %i\n" "Todo.org")
	("Journal" ?j "* %U %?\n\n  %i\n  %a" "Journal.org" "Journal")
	("Note" ?n "* %^{Title}\n  %i\n  %a" "Notes.org" "Notes")
	("Agenda" ?a "* %^{Event}\n  SCHEDULED: %^t\n  %i" "Agenda.org")
	("Home Note" ?h "* %^{Title}\n %i\n" "H:/Emacs/HomeOrg/General.org" "To File")))

;; Had a play with Bastien Guerry's Blorg Mode, but had a problem with eshell-search-path
;;(require 'blorg)

;; ----------------------------------------
;; Remember Mode
;; ----------------------------------------

(add-to-list 'load-path (concat mo-vendor-dir "/remember-2.0"))
(autoload 'remember "remember" nil t)
