(add-to-list 'load-path (concat mo-dotfiles-dir "el-get/el-get"))

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(setq el-get-sources
      '((:name auto-complete
         :website "http://cx4a.org/software/auto-complete/"
         :description "The most intelligent auto-completion extension."
         :type git
         :url "http://github.com/m2ym/auto-complete.git"
         :load-path "."
         :post-init (lambda ()
                      (require 'auto-complete)
                      (add-to-list 'ac-dictionary-directories (expand-file-name "dict" pdir))
                      (require 'auto-complete-config)
                      (ac-config-default)))
        (:name ac-dabbrev
         :type emacswiki
         :description "Emacs auto-complete plugin for Dynamic Abbreviations"
         :post-init (lambda ()
                      (require 'ac-dabbrev)
                      (setq-default ac-sources (append ac-sources '(ac-source-dabbrev)))))
        (:name ac-slime
         :website "https://github.com/purcell/ac-slime"
         :description "Emacs auto-complete plugin for Slime symbols"
         :type git
         :url "https://github.com/purcell/ac-slime.git"
         :post-init (lambda ()
                      (require 'ac-slime)
                      (add-hook 'slime-mode-hook 'set-up-slime-ac)))
        (:name auto-complete-emacs-lisp
         :description "Auto-complete sources for emacs lisp"
         :type http
         :url "http://www.cx4a.org/pub/auto-complete-emacs-lisp.el"
         :post-init (lambda ()
                      (require 'auto-complete-emacs-lisp)
                      (ac-emacs-lisp-init)))
        ;; TODO: Install rcodetools so that I can use this
        ;; (:name auto-complete-ruby
        ;;  :description "Auto-complete sources for Ruby"
        ;;  :type http
        ;;  :url "http://www.cx4a.org/pub/auto-complete-ruby.el"
        ;;  :post-init (lambda ()
        ;;               (require 'auto-complete-ruby)))
        (:name perspective
         :description "Perspectives for Emacs."
         :type git
         :url "https://github.com/deactivated/perspective-el.git"
         :post-init (lambda ()
                      (require 'perspective)
                      (persp-mode 1)))
        (:name nxhtml
         :type emacsmirror
         :description "An addon for Emacs mainly for web development."
         :build
         (list (concat el-get-emacs " -batch -q -no-site-file -L . -l nxhtmlmaint.el -f nxhtmlmaint-start-byte-compilation"))
         :load "autostart.el"
         :post-init (lambda ()
                      (add-to-list 'auto-mode-alist '("\\.ejs$" . nxhtml-mumamo-mode))))))

(setq mo-el-get-packages
      (append
       '(auto-complete
         auto-complete-emacs-lisp
         ;; auto-complete-ruby
         ac-dabbrev
         ac-slime
         perspective)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync mo-el-get-packages)
