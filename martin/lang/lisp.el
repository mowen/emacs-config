(add-to-list 'load-path (concat mo-vendor-dir "/slime"))

(require 'slime)

(slime-setup '(slime-fancy))

(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

(setq inferior-lisp-program "clisp -K full"
      lisp-indent-function 'common-lisp-indent-function ;as opposed to elisp indentation
      slime-compile-symbol-function 'slime-fuzzy-complete-symbol
      slime-to-lisp-filename-function 'mo-slime-cygwin-filename)

(defun mo-slime-cygwin-filename (filename)
  (if (eq system-type 'windows-nt) ; and lisp is cygwin
      (replace-in-string filename "\\\\" "/") filename))