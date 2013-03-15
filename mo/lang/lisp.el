;; (slime-setup '(slime-fancy))

(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook (lambda () (slime-mode t)))
(add-hook 'inferior-lisp-mode-hook (lambda () (inferior-slime-mode t)))

(if (eq system-type 'darwin)
    (setq mo-clisp-cmd "/opt/local/bin/clisp"
          mo-clisp-args "")
    (setq mo-clisp-cmd "clisp"
          mo-clisp-args "-K full"))

(setq inferior-lisp-program (concat mo-clisp-cmd " " mo-clisp-args)
      slime-lisp-implementations `((clisp (,mo-clisp-cmd ,mo-clisp-args)))
      lisp-indent-function 'common-lisp-indent-function ;as opposed to elisp indentation
      slime-compile-symbol-function 'slime-fuzzy-complete-symbol
      slime-to-lisp-filename-function 'mo-slime-cygwin-filename
      slime-protocol-version 'ignore)

(defun mo-slime-cygwin-filename (filename)
  (if (eq system-type 'windows-nt) ; and lisp is cygwin
      (replace-in-string filename "\\\\" "/") filename))

(require 's)

(defun mo-ert-for-current-file ()
  "Run ERT and select the tests for the current file. The tests must be
include the filename in order to be selected."
  (interactive)
  (let* ((filename (car (nreverse (s-split "/" (buffer-file-name)))))
         (filename-no-ext (car (s-split "\\." filename))))
    (mo-log (format "Running ERT for %s" filename-no-ext) 'debug)
    (eval-buffer)
    (ert filename-no-ext)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key emacs-lisp-mode-map (kbd "C-c t") 'mo-ert-for-current-file)))
