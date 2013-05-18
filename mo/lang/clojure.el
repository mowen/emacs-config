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
    (mo-log "loaded nrepl customisations")))
