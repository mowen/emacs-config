(durendal-enable)

;; nrepl
(eval-after-load 'nrepl
  '(progn
    (setq nrepl-popup-stacktraces nil)
    (add-to-list 'same-window-buffer-names "*nrepl*")

    (add-hook 'nrepl-interaction-mode 'paredit-mode)
    (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)

    (add-hook 'nrepl-mode-hook 'paredit-mode)
    (add-hook 'nrepl-mode-hook 'subword-mode)
    (add-hook 'nrepl-mode-hook 'clojure-mode-font-lock-setup)

    ;;(add-hook 'kill-emacs-hook '(lambda () (if (fboundp 'nrepl-quit) (nrepl-quit))))

    (require 'ac-nrepl)
    (add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
    (add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)

    (mo-log "loaded nrepl customisations")))
