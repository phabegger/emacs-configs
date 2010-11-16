(defun jone-default-python-mode-hook ()
  (set-pairs '("(" "{" "[" "\"" "\'" "`"))

  ;; additional modes
  (ftw-mode t)
  (highlight-80+-mode t)

  ;; keys
  (local-set-key (kbd "RET") 'newline-and-indent)
  (local-set-key (kbd "C-M-<return>") 'jone-python-goto-definition))

(defun jone-python-completion-mode-hook ()
  (local-set-key (kbd ")") 'jone-insert-closing-bracket-or-move-foreward))

(defun jone-python-flymake-mode-hook ()
  (when (load "flymake" t)
    (defun flymake-pyflakes-init ()
      (let* ((temp-file (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
             (local-file (file-relative-name
                          temp-file
                          (file-name-directory buffer-file-name))))
        (list "pyflakes" (list local-file))))

    (add-to-list 'flymake-allowed-file-name-masks
                 '("\\.py\\'" flymake-pyflakes-init)))

  (add-hook 'find-file-hook 'flymake-find-file-hook))

(defun jone-python-pep8-mode-hook ()
  (local-set-key (kbd "C-°") 'pep8))

(defun jone-python-buildout-find-omelette ()
  (let ((buildout-directory (find-buildout-root default-directory)))
    (let ((omelette-path (concat buildout-directory "parts/omelette/")))
      (if (file-exists-p omelette-path)
          omelette-path
        (error "Omelette not found")))))

(defun jone--python-goto-symbol (symbol)
  "Go directly to a symbol, if it exists."
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))
                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))
                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist)
      (imenu (assoc symbol name-and-pos)))))

(defun jone--follow-symlinks-recursively (path)
  "Follows symlinks of path and every parent directory."
  (let* ((realpath (or (file-symlink-p path) path))
         (parent (replace-regexp-in-string
                  "\/$" ""
                  (file-name-directory (directory-file-name realpath)))))
    (if (not (equal parent ""))
        (concat (jone--follow-symlinks-recursively parent)
                (substring realpath (length parent)))
      realpath)))

(defun jone--python-open-file (path)
  "Opens a file, which may be symlinked (or any parent).
Follows the symlink if the target is within a /src directory."
  (let ((realpath (jone--follow-symlinks-recursively path)))
    (if (search "src" realpath)
        (find-file realpath)
      (find-file path))))

(defun jone-python-open-imported-file (&optional import)
  ;; find omelette
  (let* ((omelette (jone-python-buildout-find-omelette))
         (line (or import (thing-at-point 'line)))
         (path nil)
         (import nil))

    (if (not (search "import" line))
        (error "Could not find python import at current line."))

    ;; maybe the import line imports multiple thing seperated by ","
    (if (search "," line)
        (let* ((line (split-string line "import"))
               (symbol (jone--python-string-trim (ido-completing-read "Symbol: "
                                                         (split-string
                                                          (replace-regexp-in-string
                                                           "\n" ""
                                                           (second line)) ",")))))
          (setq import (concat (first line) "import " symbol)))
      (setq import line))

    (setq path (replace-regexp-in-string
                "\\." "/"
                (replace-regexp-in-string
                 "^import " ""
                 (replace-regexp-in-string
                  " import " "."
                  (replace-regexp-in-string
                   "[ ]*from " ""
                   (replace-regexp-in-string
                    " as .*" ""
                    import))))))

    (if (not path)
        (error "Is the current line a python import?"))

    ;; strip new lines
    (setq path (replace-regexp-in-string "\n" "" path))

    ;; expect "from my.package import file""
    ;; open my/package/file.py
    (let ((expected-path (concat omelette path ".py"))
          (symbol nil))
      (message expected-path)
      (if (file-exists-p expected-path)
          (jone--python-open-file expected-path)

        ;; expect "from my.package import folder"
        ;; open my/package/folder/__init__.py
        (setq expected-path (replace-regexp-in-string
                             "\\.py" "/__init__.py" expected-path))
        (message expected-path)
        (if (file-exists-p expected-path)
            (jone--python-open-file expected-path)

          ;; expect "from my.package.folder import class"
          ;; open my/package/folder/__init__.py
          (setq symbol (let ((parts (split-string expected-path "/")))
                         (nth (- (length parts) 2) parts)))
          (setq expected-path (replace-regexp-in-string
                               "/[^/]*/__init__\\.py" "/__init__.py" expected-path))
          (message expected-path)
          (if (file-exists-p expected-path)
              (progn
                (jone--python-open-file expected-path)
                ;; go to symbol
                (if (not (jone--python-goto-symbol symbol))
                    (if (not (jone--python-goto-symbol (concat "class " symbol)))
                        (search-forward symbol))))

            ;; expect "from my.package.file import class"
            ;; open my/package/file.py
            (setq expected-path (replace-regexp-in-string
                                 "/__init__\\.py" ".py" expected-path))
            (message expected-path)
            (if (file-exists-p expected-path)
                (progn
                  (jone--python-open-file expected-path)
                  ;; go to symbol
                  (if (not (jone--python-goto-symbol symbol))
                      (if (not (jone--python-goto-symbol (concat "class " symbol)))
                          (search-forward symbol))))
              (message (concat "Not found: " expected-path)))))))))

(defun jone-python-goto (import)
  "Finds a file with its import path."
  (interactive "MPython import path: ")
  (if (not (search "import" import))
      (jone-python-open-imported-file (concat "import " import))
    (jone-python-open-imported-file import)))

(defun jone-python-goto-definition ()
  "Goes to the definition of the current word or line."
  (interactive)
  (if (search "import " (thing-at-point 'line))
      (jone-python-open-imported-file))
  (let ((symbol (current-word)))
    (if (not symbol)
        (error "Not at a import and not at a word."))
    (if (not (jone--python-goto-symbol symbol))
        (if (not (jone--python-goto-symbol (concat "class " symbol)))
            (progn (beginning-of-buffer)
                   (search-forward symbol))))))

;; helper functions

(defun jone--python-string-ltrim (str)
  (let ((trim-pos (string-match "\\s +$" str)))
    (if trim-pos
        (substring str 0 trim-pos)
      str)))

(defun jone--python-string-rtrim (str)
  (let ((trim-pos (string-match "[^ \t]+" str)))
    (if trim-pos
        (substring str trim-pos)
      str)))

(defun jone--python-string-trim (str)
  (jone--python-string-rtrim (jone--python-string-ltrim str)))


;; Python mode hooks
(add-hook 'python-mode-hook 'jone-default-python-mode-hook)
(add-hook 'python-mode-hook 'jone-python-completion-mode-hook)
(add-hook 'python-mode-hook 'jone-python-flymake-mode-hook)
(add-hook 'python-mode-hook 'jone-python-pep8-mode-hook)

