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
;; Functions for visiting today's Mayrise and Flare archive directories

(defun mo--visit-todays-arch-dir (base-dir)
  "Visit today's archive directory in BASE-DIR. The directory
name will be in the format '2008_11/12'."
  (let* ((cached-current-time (current-time))
	 (year (format-time-string "%Y" cached-current-time))
	 (month (format-time-string "%m" cached-current-time))
	 (day (format-time-string "%d" cached-current-time))
	 (arch-dir (concat base-dir year "_" month "/" day)))
    (if (file-exists-p arch-dir)
	(find-file arch-dir)
      (message "Archive directory '%s' does not exist, or is inaccessible."
	       arch-dir))))

(defmacro mo-define-visit-arch-dir-funs (archive-directories-alist)
  "Define each of the mo-visit-arch-dir functions."
  (declare (indent defun)) ;; indent the macros like defuns
  `(progn  ;; The "(progn ,@(mapcar..." is from html-lite.el. I'm not
     ,@(mapcar (lambda (directory)  ;; exactly sure how it works
		 (list 'defun (intern (format "mo-visit-%s" (car directory))) '()
		       (concat "Visit today's " (symbol-name (car directory)) " arch directory.")
		       (list 'interactive)
		       (list 'mo--visit-todays-arch-dir (cdr directory))))
	       archive-directories-alist)))

(mo-define-visit-arch-dir-funs
  ((refuse-live        . "F:/Inetpub/wwwroot/SeftonMayriseRefuse/arch/")
   (refuse-test        . "J:/Inetpub/wwwroot/SeftonMayriseRefuse/arch/")
   (refuse-test-v3     . "J:/Inetpub/wwwroot/SeftonMayriseRefuseV3/arch/")
   (tech-services-live . "F:/Inetpub/wwwroot/SeftonMayriseTechServices/arch/")
   (tech-services-test . "J:/Inetpub/wwwroot/SeftonMayriseTechServices/arch/")
   (tech-services-test-v3 . "J:/Inetpub/wwwroot/SeftonMayriseRefuse/arch/")
   (eip-live           . "P:/Inetpub/wwwroot/SeftonEIP/arch/")
   (eip-test           . "P:/Inetpub/wwwroot/SeftonEIPTest/arch/")))

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
