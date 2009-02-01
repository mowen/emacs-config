;; Emacs Lisp Utilities shared between home and work

;; Utils to Write
;;
;; - Transpose function arguments.

;; -----------------------------------------------------------------------------
;; Code to narrow to lines containing a particular regexp

(defun mo-get-regexp-positions (regexp)
  "Get a list of the positions of the given regular expression."
  (save-excursion
    (goto-char 1)
    (setq regexp-repeat 1
	  positions '())
    (while (setq pos (re-search-forward regexp nil t regexp-repeat))
      (setq positions (cons pos positions))
      (1+ regexp-repeat))
    (setq positions (nreverse positions))
    positions))

(defun mo-show-or-hide-lines (regexp-positions make-invisible)
  "Loop through all of the lines, and if one doesn't contain a regexp-position,
   set its invisiblity property to make-invisible."
  (setq num-of-lines (count-lines (point-min) (point-max)))
  (loop with i = 1 
        for i upto num-of-lines
        do
          (unless (memq i regexp-positions) ;; memq checks for membership
            (goto-line i)
            (put-text-property (line-beginning-position)
                               (1+ (line-end-position)) ;; +1 to include newline 
			       'invisible make-invisible))))

(defun mo-narrow-regexp (regexp)
  "Hide all lines but the ones which contain the input regexp."
  (interactive "sRegex: ")
  (setq regexp-lines (mapcar 
		      'line-number-at-pos 
		      (mo-get-regexp-positions regexp)))
  (mo-show-or-hide-lines regexp-lines t))

;; ------------------------------------------------------------------------------
;; Text manipulation functions

(defun mo-quote-word ()
  "Put double quotes around a word."
  (interactive)
  (forward-word)  ;; Go forward then backward
  (backward-word) ;; so that we are at start
  (insert "\"")
  (forward-word)
  (insert "\""))

(defun mo-backward-kill-word (&optional arg)
  "Special backward-kill-word function which stops deleting if it
encounters an uppercase character within the word."
  (interactive "p")
  (setq arg (or arg 1))
  (dotimes (i arg)
    (let ((case-fold-search nil) ;; Case sensitive search
	  (kill-region-end (point)))
      (save-excursion
	(setq prev-capital (or (search-backward-regexp "[A-Z]+[a-z]+" (point-min) t)
			       (line-beginning-position))))
      (save-excursion
	(setq prev-non-word (or (search-backward-regexp "\\W+\\w+" (point-min) t)
				(line-beginning-position))))
      (unless (and (not prev-non-word) (not prev-capital)) ;; can I remove this check now?
	(if (> prev-non-word prev-capital)
	    ;; If no capital before the word boundary,
	    ;; use standard backward-kill-word function
	    (backward-kill-word 1)
	  (kill-region prev-capital kill-region-end))))))

(defun mo-copy-line-as-kill (&optional arg)
  "Save the current line in the kill ring, and don't kill it."
  (interactive "p")
  (save-excursion
    (setq arg (or arg 1))
    (let ((beg (point)))
      (if (> arg 1) (forward-line (1- arg)))
      (copy-region-as-kill beg (line-end-position)))))

(defun mo-copy-word-as-kill (&optional arg)
  "Save the current word in the kill ring, and don't kill it."
  (interactive "p")
  (save-excursion
    (setq arg (or arg 1))
    (mark-word arg)
    (copy-region-as-kill (point) (mark))))

(defun mo-save-and-kill ()
  "Save the current buffer, then kill it."
  (interactive)
  (save-buffer)
  (kill-buffer nil)) ; nil argument means current-buffer

;; -----------------------------------------------------------------------------
;; html-lite helper utility

(defun mo-html-string-from-tree (html-tree)
  "Generate a string of HTML from a html-lite tree."
  (let ((html-string ""))
    (cond
     ((null (car html-tree)) '())
     ((listp (car html-tree))
      (concat (html-string-from-tree (car html-tree))
	      (html-string-from-tree (cdr html-tree))))
     (t
      (concat (car html-tree)
	      (html-string-from-tree (cdr html-tree)))))))

;;(mo-html-string-from-tree (html-div :id "martin"
;;	 			    (html-p "This is a paragraph")))
