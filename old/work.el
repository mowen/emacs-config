(defvar site-lisp "C:/Program Files/emacs-22.2/site-lisp/"
  "Location of Site Lisp code.")

(setq default-major-mode 'text-mode)
(add-hook 'text-mode-hook 
	  '(lambda ()
	     (abbrev-mode)))

(defalias 'rps 'replace-string)
(defalias 'qr 'query-replace)

;; Load up SVN support, psvn.el is in site-lisp
(require 'psvn)

;; ----------------------------------------
;; Color Themes
;; ----------------------------------------

;; Good color-themes: arjen, jsc-light2, andreas (light), clarity (a good dark one), vim-colors (light)
(add-to-list 'load-path (concat site-lisp "color-theme-6.6.0"))
(require 'color-theme)
(color-theme-initialize)
(color-theme-vim-colors)

;; ----------------------------------------
;; HTML Helper Mode
;; ----------------------------------------

;; I think I need to load Visual Basic mode.

;; html-helper-mode.el is in emacs-22.2/site-lisp
(autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)
(add-to-list 'auto-mode-alist '("\\.html$" . html-helper-mode))
(add-to-list 'auto-mode-alist '("\\.asp$" . html-helper-mode))
(add-to-list 'auto-mode-alist '("\\.jsp$" . html-helper-mode))
(add-hook 'html-helper-mode-hook 
	  '(lambda ()
	     (auto-fill-mode nil)
	     (setq html-helper-mode-uses-visual-basic t)
	     (setq html-helper-mode-global-JSP-not-ASP nil)))

;; ----------------------------------------
;; XML Mode
;; ----------------------------------------

(load (concat site-lisp "nxml-mode-20041004/rng-auto.el"))
(add-to-list 'auto-mode-alist 
	     '("\\.\\(xml\\|xsl\\|rng\\|xhtml\\|config\\|vbproj\\)\\'" . nxml-mode))

(defun mo-nxml-indent-buffer ()
  "Indent the whole buffer correctly."
  (interactive)
  (let ((xml (xml-parse-region (point-min) (point-max) (current-buffer))))
    (save-excursion
      (erase-buffer)
      (goto-char (point-min))
      (insert (xml-print xml)))))

(add-hook 'nxml-mode-hook 
	  '(lambda () 
	     (setq nxml-slash-auto-complete-flag t)
	     (define-key nxml-mode-map (kbd "C-c C-r") 'fo-remove-xml-attribute-region)
	     (define-key nxml-mode-map (kbd "C-c i") 'mo-nxml-indent-buffer)))

;; (load (concat site-lisp "nxml/autostart.el"))

;; ----------------------------------------
;; Ruby Mode Customisations
;; ----------------------------------------

(add-to-list 'load-path "C:/ruby/src/ruby-1.8.5/misc")

(require 'ruby-electric)
(autoload 'ruby-mode "ruby-mode" "Mode for editing ruby source files" t)
(add-to-list 'auto-mode-alist '("\\.\\(rb\\|gemspec\\)$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

(autoload 'run-ruby "inf-ruby" "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby" "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook 
	  '(lambda () 
	     (inf-ruby-keys)
	     (ruby-electric-mode t)
	     ;;(pabbrev-mode t)
	     (define-key ruby-mode-map "\C-c\C-a" 'ruby-eval-buffer)))

(defun ruby-eval-buffer () (interactive)
  "Evaluate the buffer with ruby."
  (shell-command-on-region (point-min) (point-max) "ruby"))

(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
;;(setq auto-mode-alist (cons '("\\.rb\\'" . rhtml-mode) auto-mode-alist))

;; ----------------------------------------
;; Steve Yegge's JS2 Mode for JavaScript
;; ----------------------------------------

;; js2.el is in site-lisp
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook 
	  '(lambda ()
	     (setq js2-bounce-indent-flag nil))) ;; Don't do the stupid thing where it cycles through indentations.

;; ----------------------------------------
;; HAML and SASS Modes
;; ----------------------------------------

(autoload 'haml-mode "haml-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(autoload 'sass-mode "sass-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

;; ----------------------------------------
;; VisualBasic.NET Mode
;; ----------------------------------------

;; vbnet-mode.el is in site-lisp
(autoload 'vbnet-mode "vbnet-mode" "Visual Basic mode." t)
(add-to-list 'auto-mode-alist '("\\.vb$" . vbnet-mode))

;; ----------------------------------------
;; Muse Mode and Planner Mode
;; ----------------------------------------
;; (add-to-list 'load-path (concat site-lisp "muse-3.12/lisp"))
;; (add-to-list 'load-path (concat site-lisp "planner-3.42"))
;; (add-to-list 'load-path (concat site-lisp "remember-2.0"))

;; (setq planner-project "WikiPlanner")
;; (setq muse-project-alist
;;       '(("WikiPlanner"
;; 	 ("~/plans"
;; 	  :default "index"
;; 	  :major-mode planner-mode
;; 	  :visit-link planner-visit-link))))
;; (require 'planner)

;; (require 'remember-planner)
;; (setq remember-handler-functions '(remember-planner-append))
;; (setq remember-annotation-functions planner-annotation-functions)

;; ----------------------------------------
;; YAML Mode
;; ----------------------------------------

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-hook 'yaml-mode-hook
	  '(lambda ()
	     (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; ----------------------------------------
;; Remember Mode
;; ----------------------------------------

(add-to-list 'load-path (concat site-lisp "remember-2.0"))
;; (autoload 'remember "remember" nil t)

;; ----------------------------------------
;; Org Mode
;; ----------------------------------------

(add-to-list 'load-path (concat site-lisp "org-6.05b/lisp"))
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(require 'org)
(setq org-directory "H:/Emacs/OrgMode/")
(setq org-default-notes-file (concat org-directory "Notes.org"))
(setq org-archive-location (concat org-directory "Archive.org::"))
(org-remember-insinuate)

(setq org-hide-leading-stars t
      org-startup-folded t
      org-log-done nil)

;; See "mo/keys.el" for Org's global key mappings

(setq org-export-html-style
      "<link rel=\"stylesheet\" type=\"text/css\" href=\"OrgStyle.css\" />")

(setq org-remember-templates
      '(("Todo" ?t "* TODO %?\n  %i\n" "Todo.org")
	("Journal" ?j "* %U %?\n\n  %i\n  %a" "Journal.org" "Journal")
	("Note" ?n "* %^{Title}\n  %i\n  %a" "Notes.org" "Notes")
	("Agenda" ?a "* %^{Event}\n  SCHEDULED: %^t\n  %i" "Agenda.org")
	("Home Note" ?h "* %^{Title}\n %i\n" "H:/Emacs/HomeOrg/General.org" "To File")
	("Lovefilm" ?l "* %i%?" "H:/Emacs/HomeOrg/General.org" "Lovefilm")
	("To Buy" ?s "* %?\n" "H:/Emacs/HomeOrg/General.org" "Shopping")
	("Books" ?b "* %?\n" "H:/Emacs/HomeOrg/General.org" "Books")))

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(add-hook 'org-after-todo-statistics-hook 'org-summary-todo)

;; ----------------------------------------
;; Textile Mode
;; ----------------------------------------

(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile$" . textile-mode))

;; ----------------------------------------
;; Multiple-Major-Modes Mode
;; ----------------------------------------

(add-to-list 'load-path (concat site-lisp "mmm-mode-0.4.8"))
(require 'mmm-mode)
(setq mmm-global-mode 'maybe)
(setq mmm-submode-decoration-level 0)
(mmm-add-mode-ext-class 'html-helper-mode nil 'embedded-css)
(mmm-add-mode-ext-class 'html-helper-mode nil 'html-js)
(mmm-add-mode-ext-class 'html-helper-mode "\\.jsp$" 'jsp)
(mmm-add-mode-ext-class 'html-helper-mode "\\.php$" 'html-php)

;; ;; Load JS2 Mode for embedded JavaScript
;; (mmm-add-group
;;  'html-js2
;;  '((js2-tag
;;     ;; Pick out standard JavaScript tags.
;;     :submode js2-mode
;;     :face mmm-code-submode-face
;;     :delimiter-mode nil
;;     :front "<script.*?javascript.*?>"
;;     :back "</script>"
;;     :insert ((?j js2-tag nil @ "<script language=\"JavaScript\">" @ "\n" _ "\n" @ "</script>" @))
;;     )
;;    (js2-inline
;;     ;; For <element onClick="var x = 10; callFunc(x);" />
;;     :submode js2-mode
;;     :face mmm-code-submode-face
;;     :delimiter-mode nil
;;     :front "on\\w+=\""
;;     :back "\"")))
;; (mmm-add-mode-ext-class 'html-helper-mode nil 'html-js2)

;; ----------------------------------------
;; Fonts
;; ----------------------------------------

;; Set default face in all frames.
;; (t instead of nil will set it only in new ones)
(set-face-attribute 'default nil :height 90 :width 'normal :family "Consolas")

;; --------------------------------------------------
;; Functions for running Textile through a Ruby filter
;; --------------------------------------------------

(defun escape-quotes (string)
  (escape-single-quotes (escape-double-quotes string)))

(defun escape-single-quotes (string)
  (replace-regexp-in-string "'" "\\'" string nil 1))

(defun escape-double-quotes (string)
  (replace-regexp-in-string "\"" "\\\"" string nil 1))

(defun ruby-on-region (rubycode)
  "This function pipes the contents of the region to a Ruby
   script and replaces the region with its output. 'rubycode'
   is a string containing the script."
  (save-excursion
    (shell-command-on-region (region-beginning) (region-end) 
                             (concat "ruby -e \"" 
                                     (escape-quotes rubycode) 
                                     "\"") 
			     -1)))

(defun textile-to-html-region ()
  "Convert textile markup in region to html. To make this work, 
  you need the ruby interpreter to be in your system path. You will
  also need to have rubygems and redcloth installed."
  (interactive "*")
  (ruby-on-region "
require 'redcloth'
text = readlines.join
puts RedCloth.new(text).to_html
"))

;; ----------------------------------------
;; SQL Mode
;; ----------------------------------------

(eval-after-load "sql"
  '(load-library "tsql-indent"))

;; ----------------------------------------
;; iBuffer

(setq ibuffer-expert t) ;; Don't ask for confirmation when deleting buffers
(setq ibuffer-saved-filter-groups
      '(("misc"
	 ("Org" (or (mode . org-mode) (name . "OrgMode")))
	 ("PBMS" (filename . "wwwroot/PBMS"))
	 ("ReleaseMe" (filename . "ReleaseMe"))
	 ("Subversion" (name . "\*svn"))
	 ("Help" (or (name . "\*Help\*")
		     (name . "\*Apropos\*")
		     (name . "\*info\*"))))
	("default"
	 ("Org" (or (mode . org-mode) (name . "OrgMode")))
	 ("Dev" (filename . "^v:/"))
	 ("Train" (filename . "^n:/"))
	 ("Live" (filename . "^o:/"))
	 ("SeftonMayrise" (filename . "wwwroot/SeftonMayrise"))
	 ("SeftonEIP" (filename . "wwwroot/SeftonEIP"))
	 ("Front Office 3 - Web" (filename . "frontoffice30/organisations/smbc/FrontOfficeWeb"))
	 ("Front Office 3 - WorkBench" (filename . "frontoffice30/organisations/smbc/WorkBench")))))

(add-hook 'ibuffer-mode-hook 
	  '(lambda ()
	     (ibuffer-switch-to-saved-filter-groups "default")))

;; ----------------------------------------
;; Dired Mode

(add-hook 'dired-mode-hook
	  '(lambda ()
	     (define-key dired-mode-map (kbd "C-c C-s")
	       '(lambda ()
		  (interactive)
		  (svn-status default-directory)))))

;; ----------------------------------------
;; CSV Mode

(require 'csv-mode)

;; ----------------------------------------
;; Bookmark

;; When opening a bookmark, name the buffer after the bookmark
(add-hook 'bookmark-after-jump-hook
	  '(lambda ()
	     (rename-buffer bookmark-current-bookmark)))

;; ----------------------------------------
;; Front Office Mode

(require 'front-office-mode) ;; My half-arsed Front Office mode
(add-to-list 'auto-mode-alist '("RapidDev.*\\.txt$" . front-office-mode))

;; My external code
(load (concat site-lisp "mo/vars.el")) ;; Variable settings
(load (concat site-lisp "mo/work-util.el")) ;; Utility functions for work
(load (concat site-lisp "mo/common-util.el")) ;; Utility functions for home and work
(load (concat site-lisp "mo/keys.el")) ;; Key bindings
(load (concat site-lisp "mo/front-office-util.el")) ;; Front Office Utilities
(load (concat site-lisp "mo/skeletons.el")) ;; My snippets

;; Ergonomic Key Map
;;(load (concat site-lisp "ergonomic_keybinding_qwerty.el"))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ibuffer-saved-filters (quote (("gnus" ((or (mode . message-mode) (mode . mail-mode) (mode . gnus-group-mode) (mode . gnus-summary-mode) (mode . gnus-article-mode)))) ("programming" ((or (mode . emacs-lisp-mode) (mode . cperl-mode) (mode . c-mode) (mode . java-mode) (mode . idl-mode) (mode . lisp-mode)))))))
 '(ido-auto-merge-work-directories-length -1)
 '(ido-create-new-buffer (quote always))
 '(ido-everywhere nil)
 '(ido-mode (quote both) nil (ido)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
