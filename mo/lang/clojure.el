(durendal-enable)
(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)

;; nrepl
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-to-list 'same-window-buffer-names "*nrepl*") 
(add-hook 'nrepl-interaction-mode 'paredit-mode)
