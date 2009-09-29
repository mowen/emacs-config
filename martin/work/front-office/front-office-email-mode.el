;; ------------------------------------------
;; Front Office V3 Email Template Minor Mode
;; 
;; Martin Owen
;;

(defvar fo3-email-mode nil
  "Mode variable for Front Office V3 Email minor mode.")

(defvar fo3-email-body-unescaped nil
  "Is the email body escaped?")

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
	(add-hook 'before-save-hook 'fo3-email-escape nil t))
    (remove-hook 'post-command-hook 'fo3-email-unescape t)
    (remove-hook 'before-save-hook 'fo3-email-escape t)))

(if (not (assq 'fo3-email-mode minor-mode-alist))
    (add-to-list 'minor-mode-alist '(fo3-email-mode " FO V3 Email"))
  minor-mode-alist)

(defun fo3-email-escape ()
  "Escape the <emailBody>."
  (if fo3-email-body-unescaped
      (progn
	(let ((buffer-modified (buffer-modified-p)))
	  (fo3-remove-newlines-after-paragraphs)
	  (fo3-escape-email-template-html)
	  (set-buffer-modified-p buffer-modified)))))

(defun fo3-email-unescape ()
  "Unescape the <emailBody>."
  (if (not fo3-email-body-unescaped)
      (progn
	(fo3-unescape-email-template-html)
	(fo3-add-newlines-after-paragraphs)
	(set-buffer-modified-p nil))))

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
  (let* ((buffer-modified (buffer-modified-p))
	 (initial-beg (fo3-email-body-start))
	 (initial-end (fo3-email-body-end))
	 (end initial-end))
    (setq end (+ end (fo3-replace-string ">" "&gt;" initial-beg end)))
    (setq end (+ end (fo3-replace-string "<" "&lt;" initial-beg end)))
    (fo3-replace-string "&" "&amp;" initial-beg end)
    (setq fo3-email-body-unescaped nil)))

(defun fo3-add-newlines-after-paragraphs ()
  "Insert a newline after every </P> end tag."
  (fo3-replace-string "</P>" "</P>
" (fo3-email-body-start) (fo3-email-body-end)))

(defun fo3-remove-newlines-after-paragraphs ()
  "Remove the newlines after every </P> end tag."
  (fo3-replace-string "</P>
" "</P>" (fo3-email-body-start) (fo3-email-body-end)))

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

(provide 'fo3-email-mode)