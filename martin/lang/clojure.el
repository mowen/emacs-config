(durendal-enable)
(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)
(add-hook 'clojure-mode-hook '(lambda () (paredit-mode +1)))