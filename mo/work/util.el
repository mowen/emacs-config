;; Emacs Lisp utilities used in work

;; -----------------------------------------------------------------------------
;; Decimal time functions

(defun mo-convert-decimal-time (decimal-time)
  "Convert the second element of a (hours minutes) list from decimal
 minutes to actual minutes. Used for filling in timesheets."
  (let* ((hours (truncate decimal-time))
	 (minutes (truncate
		   (* 100.0 (- decimal-time hours)))))
    (setq minutes (truncate (* minutes (/ 6.0 10.0))))
    `((hours ,hours) (minutes ,minutes))))

(defun mo-convert-time (time-alist)
  "Convert minutes to decimal."
  (setq minutes (cadr (assoc 'minutes time-alist)))
  (assq-delete-all 'minutes time-alist)
  (add-to-list 'time-alist
	       `(minutes . ,(truncate (* minutes (/ 10.0 6.0))))))

;; -----------------------------------------------------------------------------
;; Execute the region of SQL
;;
;; Investigate SQLi (executed by sql-ms), as that may do this for me.

;; mo-connection-strings-hash is defined in passwords.el

(defun mo--build-osql-command-line (connection-key)
  "Retrieve entry for key in connection strings hash, and build
command line from it."
  (let* ((connection-string-details
	  (gethash connection-key mo-connection-strings-hash))
	 (server (cdr (assoc 'server connection-string-details)))
	 (database (cdr (assoc 'database connection-string-details)))
	 (username (cdr (assoc 'username connection-string-details)))
	 (password (cdr (assoc 'password connection-string-details))))
    (message (format "osql -S %s -d %s -U %s -P %s" server database username password))))

(defun mo-osql-on-region (server-database)
  "This function pipes the contents of the region to an instance
of the 'osql' Microsoft SQL Server command-line tool."
  (interactive "sWhich server/database?")
  (setq server-database (or server-database 'crm3-sefton))
  (setq command-line (mo--build-osql-command-line server))
  (save-excursion
    (shell-command-on-region (region-beginning) (region-end) 
                             command-line -1)))

;; -----------------------------------------------------------------------------
;; One day set global-mode-string to show decimal time

;; How to set the global mode string
;; (setq global-mode-string
;;      (append global-mode-string "martin"))
