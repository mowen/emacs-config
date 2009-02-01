;; ----------------------------------------
;; PHP Mode
;; ----------------------------------------

(require 'php-mode)

;; Drupal filename extensions
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.module$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.install$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.engine$" . php-mode))
