(add-to-list 'load-path (concat mo-vendor-dir "/eproject"))
(require 'eproject)
(require 'eproject-extras)

;; eproject global bindings
(defmacro emacs-curry (function &rest args)
  `(lambda ()
     (interactive)
     (,function ,@args)))

(defmacro emacs-eproject-key (key command)
  (cons 'progn
        (loop for (k . p) in (list (cons key 4) (cons (upcase key) 1))
              collect
              `(global-set-key
                (kbd ,(format "C-x p %s" k))
                (emacs-curry ,command ,p)))))

(emacs-eproject-key "k" eproject-kill-project-buffers)
(emacs-eproject-key "v" eproject-revisit-project)
(emacs-eproject-key "o" eproject-open-all-project-files)
(emacs-eproject-key "t" eproject-todo)

(define-project-type emacs-config (generic-git)
  (look-for "init.el")
  :relevant-files ("\.el$")
  :irrelevant-files ("vendor/")
  :project-name (lambda (root) "Emacs Config"))