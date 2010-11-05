(require 'json)

(defun ftw-bla ()
  (interactive)
  (let* ((resp (replace-regexp-in-string
                "\n$" ""
                (shell-command-to-string "~/.emacs.d/sbin/create_bla.py")))
         (data (split-string resp ": ")))
    (if (and (= 2 (length data))
             (equal (nth 0 data) "file"))
        (progn
          (find-file (nth 1 data))
          (goto-line 24)
          (message (nth 1 data)))
      (error resp))))

(defun ftw-changelog ()
  (interactive)
  (goto-line 7)
  (textmate-next-line)
  (textmate-next-line)
  (insert "  [")
  (insert "jbaumann]")
  (newline)
  (previous-line)
  (previous-line)
  (insert "* "))

(defun ftw-open-changelog-make-entry ()
  (interactive)
  (find-file (concat (find-parent-with-file default-directory "setup.py") "docs/HISTORY.txt"))
  (ftw-changelog))

(defun ftw-zope-foreground ()
  (interactive)
  (call-interactively 'run-application-based-on-buffer))

(defun ftw-zope-instane (opts)
  (interactive "MOptions:")
  (compile (concatenate 'string "ftw zi " opts)))

(defun ftw-zope-kill ()
  (interactive)
  (call-interactively 'kill-compilation))

(defun ftw-zope-test ()
  (interactive)
  (compile "ftw test"))

(defun ftw-zope-i18n-build ()
  (interactive)
  (shell-command "ftw i18npot"))

(defun ftw-zope-i18n-sync (lang)
  (interactive "MLang: ")
  (let ((output (shell-command-to-string
                 (concatenate 'string "ftw i18nsync " lang))))
    (print output)
    (if (string-match "\/.*.po" output)
        (find-file (match-string 0 output)))))

(defun ftw-zope-reload-code (opts)
  (interactive "MOptions [-c, -z, -u, -p, -H, -P]:")
  (shell-command (concatenate 'string "~/.emacs.d/sbin/zope_reload_code.py " opts)))

(defun ftw-mrsd-reload ()
  (interactive)
  (let* ((instances (json-read-from-string
                     (shell-command-to-string "mrsd reload --list-instances")))
         (instance (ido-completing-read "Instance: " (coerce instances 'list))))
    (shell-command (concat "mrsd reload --instance " instance))))

(defun ftw-zope-buildout (opts)
  (interactive "MOptions:")
  (call-interactively 'run-buildout-based-on-buffer))

;; (defvar ftw-mode-map
(setq ftw-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c f f") 'ftw-zope-foreground)
        (define-key map (kbd "C-c f k") 'ftw-zope-kill)
        (define-key map (kbd "C-c f t") 'ftw-zope-test)
        (define-key map (kbd "C-c f i") 'ftw-zope-i18n-build)
        (define-key map (kbd "C-c f C-i") 'ftw-zope-i18n-sync)
        (define-key map (kbd "C-c f b") 'ftw-zope-buildout)
        (define-key map (kbd "C-c f C-b") 'ftw-bla)
        (define-key map (kbd "C-c f r") 'ftw-zope-reload-code)
        (define-key map (kbd "C-c f C-r") 'ftw-mrsd-reload)
        (define-key map (kbd "C-c f z") 'ftw-zope-instane)
        (define-key map (kbd "C-c f c") 'ftw-open-changelog-make-entry)
        (define-key map (kbd "C-c f s") 'jone-reload-safari)
        map
        ))


(define-minor-mode ftw-mode "" nil " FTW" ftw-mode-map)
