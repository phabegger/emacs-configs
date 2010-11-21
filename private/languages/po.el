;; enable po-mode for .po-files (internationalization)
(vendor 'po-mode)
(add-to-list 'auto-mode-alist '("\\.po$" . po-mode))
(add-to-list 'auto-mode-alist '("\\.pot$" . po-mode))
