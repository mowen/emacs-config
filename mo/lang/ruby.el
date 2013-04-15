;; ----------------------------------------
;; Ruby Mode
;; ----------------------------------------

(defun ruby--jump-to-test ()
  (find-file
   (replace-regexp-in-string
    "/lib/" "/test/"
    (replace-regexp-in-string
     "/\\([^/]+\\).rb$" "/test_\\1.rb"
     (buffer-file-name)))))

(defun ruby--jump-to-lib ()
  (find-file
   (replace-regexp-in-string
    "/test/" "/lib/"
    (replace-regexp-in-string
     "/test_\\([^/]+\\).rb$" "/\\1.rb"
     (buffer-file-name)))))

(defun ruby-jump-to-other ()
  (interactive)
  (if (string-match-p "/test/" (buffer-file-name))
      (ruby--jump-to-lib)
    (ruby--jump-to-test)))

(eval-after-load 'ruby-mode
  '(progn
    (require 'ruby-electric)
    (require 'ruby-block)
    (require 'inf-ruby)

    (add-hook 'ruby-mode-hook
     '(lambda ()
       (local-set-key [f1] 'yari)
       (ruby-block-mode t)
       (setq ruby-block-highlight-toggle t)
       ;; Fix silly indentation
       (setq ruby-deep-indent-paren nil)
       (setq ruby-deep-indent-paren-style nil)
       (setq tab-width 2)
       (set-variable (make-variable-buffer-local 'whitespace-tab-width) 2)
       (esk-paredit-nonlisp)
       (esk-prog-mode-hook)))

    (add-hook 'ruby-mode-hook 'inf-ruby-setup-keybindings)

    (add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))
    (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

    (define-key ruby-mode-map (kbd "C-c t") 'ruby-jump-to-other)

    (defadvice ruby-indent-line (after unindent-closing-paren activate)
     (let ((column (current-column))
           indent offset)
       (save-excursion
         (back-to-indentation)
         (let ((state (syntax-ppss)))
           (setq offset (- column (current-column)))
           (when (and (eq (char-after) ?\))
                      (not (zerop (car state))))
             (goto-char (cadr state))
             (setq indent (current-indentation)))))
       (when indent
         (indent-line-to indent)
         (when (> offset 0) (forward-char offset)))))

    (mo-log "ruby-mode customisations loaded")))
