;; ----------------------------------------
;; Ruby Mode
;; ----------------------------------------

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
       (esk-prog-mode-hook)))

    (add-hook 'ruby-mode-hook 'inf-ruby-setup-keybindings)

    (add-to-list 'auto-mode-alist '("Guardfile" . ruby-mode))
    (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))

    ;; Prevent indentation from being added after a paren
    (setq ruby-deep-indent-paren nil)
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
