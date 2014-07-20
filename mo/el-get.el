(add-to-list 'load-path (concat mo-dotfiles-dir "el-get/el-get"))

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(setq el-get-sources
      '((:name auto-complete-emacs-lisp
         :description "Auto-complete sources for emacs lisp"
         :type http
         :url "http://www.cx4a.org/pub/auto-complete-emacs-lisp.el"
         :post-init (progn
                      (require 'auto-complete-emacs-lisp)
                      (ac-emacs-lisp-init)))
        (:name perspective
         :description "Perspectives for Emacs."
         :type git
         :url "https://github.com/deactivated/perspective-el.git"
         :post-init (progn
                      (require 'perspective)
                      (persp-mode 1)))))

(setq mo-el-get-packages
      (append
       '(auto-complete-emacs-lisp
         perspective)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync mo-el-get-packages)
