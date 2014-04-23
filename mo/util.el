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

(defun mo-indent-defun ()
  "Indent the current function."
  (interactive)
  (save-excursion
    (mark-defun)
    (indent-region (region-beginning)
                   (region-end))))

(defun mo-create-temp-buffer ()
  "Create a temp buffer."
  (interactive)
  (let ((temp-buffer (get-buffer-create "temp")))
    (switch-to-buffer temp-buffer)
    (rename-uniquely)
    (text-mode)))

(defun mo-list-packages (&optional arg)
  (interactive "p")
  (cond ((= 1 arg) (package-list-packages-no-fetch))
        (t (package-list-packages))))

(defun indent-buffer ()
  "Re-indent the entire buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun google-region ()
  "Google the selected region"
  (interactive)
  (let ((query (buffer-substring (region-beginning) (region-end))))
    (browse-url (concat "http://www.google.com/search?ie=utf-8&oe=utf-8&q=" query))))

(defun so-region ()
  "Search Stack Overflow with contents of the selected region"
  (interactive)
  (let ((query (buffer-substring (region-beginning) (region-end))))
    (browse-url (concat "http://stackoverflow.com/search?q=" query))))

(defun font-size-modify (operator &optional amount)
  "Increase the size of the default font."
  (let ((height (face-attribute 'default :height))
        (amount (or amount 10)))
    (set-face-attribute 'default nil :height (funcall operator height amount))
    (message (format "Face height is now %d." height))))

(defun toggle-window-split ()
  "Toggles between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                  'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defmacro after (mode &rest body)
  "`eval-after-load' MODE evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,mode
     '(progn ,@body)))

(defun mo-format-timestamps ()
  (interactive)
  (font-lock-add-keywords
   nil
   '(("[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]"
      (0 `(face nil display
                ,(format-time-string "%F %T"
                                     (seconds-to-time
                                      (car
                                       (read-from-string
                                        (concat
                                         (match-string 0)
                                         ".0")))))))))))

(defun uniquify-region-lines (beg end)
    "Remove duplicate adjacent lines in region."
    (interactive "*r")
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
        (replace-match "\\1"))))
  
  (defun uniquify-buffer-lines ()
    "Remove duplicate adjacent lines in the current buffer."
    (interactive)
    (uniquify-region-lines (point-min) (point-max)))
