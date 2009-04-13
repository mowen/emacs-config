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

(defun mo-javascript-include (filename)
  "Script tag to include the given Javascript FILENAME."
  (format "<script type=\"text/javascript\" language=\"javascript\" src=\"%s\"></script>\n"
	  filename))

(setq org-export-html-style-include-scripts nil
      org-export-html-style-include-default nil)
(setq org-export-html-style
      (concat
       (mo-javascript-include "jorg/jquery-1.2.6.min.js")
       (mo-javascript-include "jorg/jorg.js")
       (mo-css-include "jorg/jorg.css")))

;; See "bindings.el" for Org's global key mappings

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
