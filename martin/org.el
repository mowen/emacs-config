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

(setq mo-org-home-dir (if (eq mo-location 'work)
			  "H:/Emacs/HomeOrg/" ;; I store my home org files separately when I'm in work.
			org-directory))
(setq org-remember-templates
      `(("ABC" ?a "* %T\n   - A. %^{Event}\n   - B. %^{Belief}\n   - C. %^{Consequence}\n" ,(concat mo-org-home-dir "General.org") "ABC")))

;; ----------------------------------------
;; Remember Mode
;; ----------------------------------------

(add-to-list 'load-path (concat mo-vendor-dir "/remember-2.0"))
(autoload 'remember "remember" nil t)
