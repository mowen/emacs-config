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
;; ido-menu - Taken from emacs-starter-kit

(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))
                              
                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))
                              
                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))
                             
                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((default-symbol (if (member (current-word) symbol-names) (current-word)))
	   (selected-symbol (ido-completing-read "Symbol? " symbol-names nil nil nil nil default-symbol))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))

(defun indent-buffer ()
  "Re-indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

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
