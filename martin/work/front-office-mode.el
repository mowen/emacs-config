;; Documentation for Major Modes is on page 120 of Writing GNU Emacs Extensions

(defvar front-office-mode-hook nil
  "List of functions to call when entering Front Office mode.")

(defvar front-office-mode-map nil
  "Keymap for Front Office Major mode.")

(defvar front-office-main-table-delimiter "*]"
  "String that separates Front Office tables in Rapid Dev templates.")

(defvar front-office-mail-table-delimiter "~"
  "String that separates emails in Rapid Dev mail templates.")

(defvar front-office-auto-update-table-delimiter "]"
  "String that separates entries in Auto Updates.")

(defvar front-office-control-delimiter "^"
  "String that separates Front Office controls in Rapid Dev templates.")

(defvar front-office-newline "
" "Newline string.")

(if front-office-mode-map 
    nil
  (setq front-office-mode-map (make-sparse-keymap))
  (define-key front-office-mode-map "\C-c\j" 'front-office-join-template)
  (define-key front-office-mode-map "\C-c\s" 'front-office-split-template))

;; (defvar front-office-mode-syntax-table
;;   (let ((table (make-syntax-table)))
;;     (modify-syntax-entry ?~ "." table)
;;     (modify-syntax-entry ?* "w" table)
;;     table)
;;   "Syntax table in use in front-office-mode buffers.")

(defun front-office--mail-template-p ()
  "Does this buffer contain a mail template?"
  (not (null (string-match "mail.*\.txt" buffer-file-name))))

(defun front-office--auto-update-p ()
  "Does this buffer contain an Auto Update template?"
  (not (null (string-match "RapidDev/AutoUpdate/.*\.txt" buffer-file-name))))

(defun front-office--select-delimiter ()
  "Select whether or not to use the mail template delimiter." 
  (cond
   ((front-office--mail-template-p) front-office-mail-table-delimiter)
   ((front-office--auto-update-p) front-office-auto-update-table-delimiter)
   (t front-office-main-table-delimiter)))

(defun front-office-split-template ()
  "Split a Front Office template by inserting newlines after the delimiter."
  (interactive)
  (let* ((delimiter (front-office--select-delimiter))
	 (from delimiter)
	 (to (concat delimiter front-office-newline)))
    (cond 
     ((null mark-active) (replace-string from to))
     (t (replace-string from to nil (region-beginning) (region-end))))))

(defun front-office-join-template ()
  "Join a Front Office template by removing newlines after the delimiter."
  (interactive)
  (let* ((delimiter (front-office--select-delimiter))
	 (from (concat delimiter front-office-newline))
	 (to delimiter))
    (cond
     ((null mark-active) (replace-string from to))
     (t (replace-string from to nil (region-beginning) (region-end))))))

(defun front-office-split-controls ()
  "Split the Controls in a Front Office table line by adding newlines after '^' chars."
  (interactive)
  (let ((from front-office-control-delimiter)
	(to (concat front-office-control-delimiter front-office-newline)))
    (replace-string from to nil (region-beginning) (region-end))))

(defun front-office-join-controls ()
  "Join the Controls in a Front Office table line by removing newlines after '^' chars."
  (interactive)
  (let ((from (concat front-office-control-delimiter front-office-newline))
	(to front-office-control-delimiter))
    (replace-string from to nil (region-beginning) (region-end))))

(defun front-office-hide-dfh-control ()
  "Change the flags on a Data From Header control line so that they are hidden."
  (interactive)
  (replace-string "|}N|N||N|A|"
		  "|N/A}N|N||Y|A|"
		  nil
		  (region-beginning)
		  (region-end)))

(defun front-office-mode ()
  "Major mode for editing Front Office templates.

\\{front-office-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'front-office-mode)
  (setq mode-name "Front Office")
  (use-local-map front-office-mode-map)
  (run-hooks front-office-mode-hook))

(provide 'front-office-mode)