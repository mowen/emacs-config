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