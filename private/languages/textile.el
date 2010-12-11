(vendor 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile$" . textile-mode))

;; Hooks
(defun default-textile-mode-hook ()
  (set-pairs '("[" "{")))

(add-hook 'textile-mode-hook 'default-textile-mode-hook)
