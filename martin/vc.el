(require 'psvn)
(require 'magit)

(defun mo-vc-directory-p (dir vc-dir)
  "Does DIR contain a version control directory?"
  (let ((full-vc-dir (concat (file-name-as-directory dir) vc-dir)))
    (file-directory-p full-vc-dir)))

(defun mo-context-sensitive-vc-status (dir)
  "Run version control status on DIR."
  (cond
   ((mo-vc-directory-p dir ".svn")
    (svn-status dir))
   ((mo-vc-directory-p dir ".git")
    (magit-status dir))
   (t (message
       (format "Directory '%s' is not version controlled." dir)))))