;; ----------------------------------------
;; Logging Utilities

(eval-when-compile
  (require 'cl))

(require 'eieio)

(defclass mo-log/buffer-object ()
  ((level :initarg :level :initform mo-log/info-level)
   (buffer :initarg :buffer :initform "**mo-log**")))

(defmethod mo-log/should-log ((log mo-log/buffer-object) level)
  (let ((l (oref log :level)))
    (and (oref log :buffer)
         (integerp (symbol-value l))
         (<= (symbol-value level) (symbol-value l)))))

(defmethod mo-log/insert-log ((log mo-log/buffer-object) format &rest objects)
  (let ((buffer (get-buffer-create (oref log :buffer))))
    (with-current-buffer buffer
      (goto-char (point-max))
      (insert (apply 'format format objects) "\n"))))

(defmethod mo-log/log ((log mo-log/buffer-object) level tag string &rest objects)
  (message (format "%d" (symbol-value level)))  
  (when (mo-log/should-log log level)
    (apply 'mo-log/insert-log log (format "[%s] %s" tag string) objects)))

(defmacro mo-log/def-level (sym val &optional pkg)
  "Define a constant mo-log/<SYM>-level and a macro mo-log/<SYM> associated with this level."
  (let* ((pkg (or pkg 'mo-log))
         (const (intern (format "%s/%s-level"
                                (symbol-name pkg) (symbol-name sym))))
         (mac (intern (format "%s/%s"
                              (symbol-name pkg) (symbol-name sym)))))
    `(progn
       (defconst ,const ,val)
       (defmacro ,mac (log string &rest objects)
         (append
          (list 'mo-log/log log ,const '',sym string)
          objects)))))

;; Log levels
(mo-log/def-level error 0)
(mo-log/def-level info 5)
(mo-log/def-level verbose 10)
(mo-log/def-level debug 15)

(defun mo-init-logger (&optional level)
  "Initialize a logger with the given level"
  (let ((log-level (intern (format "mo-log/%s-level" (symbol-name (or level 'info))))))
    (setq mo-logger (make-instance 'mo-log/buffer-object :level log-level))))

(defun mo-log (message &optional level)
  "Log message if we are logging at level. Default level is info."
  (let* ((log-level-string (symbol-name (or level 'info)))
         (log-level (intern (format "mo-log/%s-level" log-level-string)))
         (time-string (format-time-string "%Y/%m/%d %H:%M:%S.%3N" (current-time)))
         (tag (format "%s %s" time-string (upcase log-level-string))))
    (mo-log/log mo-logger log-level tag message)))

(defun mo-log-buffer (&optional level)
  "Log the contents of the entire buffer."
  (mo-log (buffer-substring (point-min) (point-max)) level))

(mo-init-logger 'debug)

;; Logging Tests

(ert-deftest logger-should-log-error ()
  (mo-init-logger 'verbose)
  (should (mo-log/should-log mo-logger 'mo-log/error-level)))

(ert-deftest logger-should-not-log-debug ()
  (mo-init-logger 'verbose)
  (should (not (mo-log/should-log mo-logger 'mo-log/debug-level))))

;; ----------------------------------------
;; Init Logging

(eval-after-load 'starter-kit-autoloads
  '(progn
    (mo-log "starter-kit loaded")))

(eval-after-load 'starter-kit-ruby-autoloads
  '(progn
    (mo-log "starter-kit-ruby loaded")))

