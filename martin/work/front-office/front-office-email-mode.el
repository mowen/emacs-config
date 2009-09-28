;; ------------------------------------------
;; Front Office V3 Email Template Minor Mode
;; 
;; Martin Owen
;;

(defvar fo3-email-mode nil
  "Mode variable for Front Office V3 Email minor mode.")

(defvar fo3-email-body-unescaped nil
  "Is the email body escaped?")

(defvar fo3-html-start
  (concat 
   "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n"
   "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
   "<head>\n"
   "<meta http-equiv=\"Content-type\" content=\"text/html; charset=utf-8\" />\n"
   "<title>HTML Email Temp Edit</title>\n"
   "</head>\n"
   "<body>\n")
  "Start of HTML temp page for emails.")

(defvar fo3-html-end 
  (concat
   "</body>\n"
   "</html>")
  "End of HTML temp page for emails.")

(defconst fo3-email-body-tag "emailBody"
  "The name of the tag for the email body.")

(make-variable-buffer-local 'fo3-email-mode)
(make-variable-buffer-local 'fo3-email-body-unescaped)

(defun fo3-email-mode (&optional arg)
  "Front Office V3 Email Template minor mode."
  (interactive "P")
  (setq fo3-email-mode
 	(if (null arg)
 	    (not fo3-email-mode)
 	  (> (prefix-numeric-value arg) 0)))
  (if fo3-email-mode
      (progn
	(add-hook 'post-command-hook 'fo3-email-unescape nil t)
	(add-hook 'before-save-hook 'fo3-email-escape nil t)
	(add-hook 'kill-buffer-hook 'fo3-email-escape nil t))
    (remove-hook 'post-command-hook 'fo3-email-unescape t)
    (remove-hook 'before-save-hook 'fo3-email-escape t)
    (remove-hook 'kill-buffer-hook 'fo3-email-escape t)))

(if (not (assq 'fo3-email-mode minor-mode-alist))
    (add-to-list 'minor-mode-alist '(fo3-email-mode " FO V3 Email"))
  minor-mode-alist)

(defun fo3-email-escape ()
  "Escape the <emailBody>."
  (if fo3-email-body-unescaped
      (progn
	(fo3-remove-newlines-after-paragraphs)
	(fo3-escape-email-template-html))))

(defun fo3-email-unescape ()
  "Unescape the <emailBody>."
  (if (not fo3-email-body-unescaped)
      (progn
	(fo3-unescape-email-template-html)
	(fo3-add-newlines-after-paragraphs))))
;;	(fo3-edit-email-body-in-indirect-buffer))))

(defun fo3-email-body-start ()
  "Find the <emailBody> start tag."
  (save-excursion
    (goto-char 1)
    (re-search-forward (concat "<" fo3-email-body-tag ">"))))

(defun fo3-email-body-end ()
  "Find the </emailBody> end tag."
  (save-excursion
    (goto-char (point-max))
    (re-search-backward (concat "</" fo3-email-body-tag ">"))))

(defun fo3-unescape-email-template-html ()
  "Unescape the HTML in a Front Office V3 Email template."
  (let* ((initial-beg (fo3-email-body-start))
	 (initial-end (fo3-email-body-end))
	 (end initial-end))
    (setq end (+ end (fo3-replace-string "&amp;" "&" initial-beg end)))
    (setq end (+ end (fo3-replace-string "&lt;" "<" initial-beg end)))
    (fo3-replace-string "&gt;" ">" initial-beg end)
    (setq fo3-email-body-unescaped t)))

(defun fo3-escape-email-template-html ()
  "Escape the HTML in a Front Office V3 Email template."
  (let* ((initial-beg (fo3-email-body-start))
	 (initial-end (fo3-email-body-end))
	 (end initial-end))
    (setq end (+ end (fo3-replace-string ">" "&gt;" initial-beg end)))
    (setq end (+ end (fo3-replace-string "<" "&lt;" initial-beg end)))
    (fo3-replace-string "&" "&amp;" initial-beg end)
    (setq fo3-email-body-unescaped nil)))

(defun fo3-add-newlines-after-paragraphs ()
  "Insert a newline after every </P> end tag."
  (replace-string "</P>" "</P>
" nil (fo3-email-body-start) (fo3-email-body-end)))

(defun fo3-remove-newlines-after-paragraphs ()
  "Remove the newlines after every </P> end tag."
  (replace-string "</P>
" "</P>" nil (fo3-email-body-start) (fo3-email-body-end)))

(defun fo3-replace-string (from-string to-string beg end)
  "Replace a string."
  ;; Need to count the number of chars that we add or remove, so that the
  ;; calling function can calculate the new end of the region.
  ;; This works for now but there must be a better way of doing it?
  (let ((char-difference (- (length to-string) (length from-string)))
	(chars-modified 0))
    (save-excursion
      (save-restriction
	(narrow-to-region beg end)
	(goto-char 1)
	(while (search-forward from-string nil t)
	  (setq chars-modified (+ chars-modified char-difference))
	  (replace-match to-string nil t)))
      chars-modified)))

(defun fo3-edit-email-body-in-indirect-buffer ()
  "Create an indirect buffer for editing the Email Body as HTML."
  (let ((email-body-start (fo3-email-body-start))
	(email-body-end (fo3-email-body-end))
	(new-buffer-name (concat (buffer-name) "-email-body")))
    (make-indirect-buffer (current-buffer) new-buffer-name nil)
    (switch-to-buffer new-buffer-name)
    (narrow-to-region email-body-start email-body-end)
;;    (fo3-insert-html-wrapper)
    (html-mode)))
;;    (add-hook 'kill-buffer-hook
;;	      '(lambda () (fo3-delete-html-wrapper)) nil t)
;;    (add-hook 'before-save-hook
;;	      '(lambda () (fo3-delete-html-wrapper)) nil t)))

(defun fo3-insert-html-wrapper ()
  "Insert the HTML start and end."
  (save-excursion
    (goto-char (point-min))
    (insert fo3-html-start)
    (goto-char (point-max))
    (insert fo3-html-end)))

(defun fo3-delete-html-wrapper ()
  "Remove the HTML start and end."
  (save-excursion
    (goto-char (point-min))
    (delete-char (length fo3-html-start))
    (goto-char (point-max))
    (delete-char (- (length fo3-html-end)))))

(provide 'fo3-email-mode)