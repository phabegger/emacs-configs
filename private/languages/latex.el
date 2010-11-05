;; LaTeX


;; Flymake mode for LaTeX
;; it is not enabled by default, but it is ready to go.
(defun flymake-get-tex-args (file-name)
  (list "chktex" (list "-q" "-v0" file-name)))

(add-hook 'latex-mode-hook
          '(lambda ()
             (set-pairs '("(" "{" "[" "\"" "\'"))
             (auto-fill-mode 1)
             (flymake-mode t)))
