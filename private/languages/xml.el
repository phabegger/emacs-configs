(add-hook 'nxml-completion-hook 'rng-complete nil t)
(setq rng-nxml-auto-validate-flag t)
(add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.zcml$" . nxml-mode))

;; nXhtml
(add-hook 'html-mode-hook '(lambda ()
                             (make-variable-buffer-local font-lock-function-name-face)
                             (setq font-lock-function-name-face '((t (:inherit keyword))))))
