;; -----------------------------------------------------------------------------
;; Front Office Functions

(defun fo-remove-xml-attribute (&optional arg)
  "Remove the \"FO\" attribute from an XML element in a Front
  Office template."
  (interactive "p")
  (setq arg (or arg 1))
  (dotimes (i arg)
    (beginning-of-line)
    (nxml-forward-balanced-item)
    (backward-char 1)
    (zap-to-char -2 ?\s) ;; delete to second space behind
    (forward-line 1)))

(defun fo-remove-xml-attribute-region (start end)
  "Remove the \"FO\" attributes on each of the lines in the
  region."
  (interactive "r")
  (let ((num-of-lines (count-lines start end)))
    (save-excursion
      (goto-char start)
      (fo-remove-xml-attribute num-of-lines))))

(defun fo-replace-windows-newlines ()
  "Simple function to remove \"\" characters."
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "" nil t)
      (replace-match "" nil t))))

(defun fo-remove-first-char ()
  "Remove the first character of the document. Used to remove the
funny character that seems to appear at the start of all Front
Office XML documents."
  (save-excursion
    (goto-char 2)
    (backward-delete-char 1)))
   
(defun fo-tidy-xml ()
  "Calls both of the Front Office XML tidy up functions."
  (interactive)
  (fo-replace-windows-newlines)
  (fo-remove-first-char))

(defvar fo-front-office-working-copy-dir
  "C:/\"Documents and Settings\"/OWENMAR/\"My Documents\"/\"Subversion Working Copies\"/smbc"
  "My Front Office working copy.")

(defmacro fo-define-copy-activity-xml-funs (fo-front-office-dirs-alist)
  "Define each of the fo-copy-activity-xml functions."
  (declare (indent defun)) ;; indent the macros like defuns
  `(progn
     ,@(mapcar (lambda (directory)
		 (list 'defun (intern (format "fo-copy-activity-xml-from-%s" (car directory))) '(activity-id)
		       (concat "Copy activity XML from " (symbol-name (car directory)))
		       (list 'interactive "sActivity ID: ")
		       (list 'fo--copy-activity-xml 'activity-id (cdr directory))))
	       fo-front-office-dirs-alist)))

(fo-define-copy-activity-xml-funs
  ((dev   . "L:/frontoffice30/organisations/smbc")
   (train . "U:/frontoffice30/organisations/smbc")
   (live  . "M:/frontoffice30/organisations/smbc")))

(defun fo--copy-activity-xml (activity-id src-smbc-directory)
  "Copy the activity-xml for ACTIVITY-ID to directory at SRC-LOCATION."
  (let* ((activity-xml-filename (concat (upcase activity-id) ".xml"))
	 (workbench-activities-dir "/Workbench/xml/Activities/")
	 (src (concat src-smbc-directory
		      workbench-activities-dir
		      activity-xml-filename))
	 (dest (concat fo-front-office-working-copy-dir
		       workbench-activities-dir
		       activity-xml-filename)))
    (fo--copy src dest)))

(defun fo--copy (src dest)
  "Copy SRC to DEST."
  (shell-command-to-string (format "cp %s %s" src dest)))
