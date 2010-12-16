(defmacro directory-excursion (dir &rest body)
  "Run all BODY commands within the DIR directory and restore the
current working dir after completion.d "
  `(let ((old-directory default-directory))
     (progn (cd ,dir) ,@body (cd old-directory) nil)))

(defun run-command (command &optional buffer)
  "Run a COMMAND, it can be a string with space seperated arguments

You can also specify a BUFFER for the output. By default it is
set to nil. You can set it to 0 to make the process
detached. Another option is to set it to a string. It will then
create a buffer with that name. The last option is setting it to
t, this appends the output to the current buffer.
"
  (interactive "sCommand: ")
  (if (eq 0 (let ((programm (split-string command " " t)))
              (apply 'call-process (car programm) nil buffer t (cdr programm))))
      (message "Command completed: %s" command)
    (message "Problem running: %s" command)))

(defun find-parent-with-file (path filename)
  "Traverse PATH upwards until we find FILENAME in the dir.

If we find it return the path of that dir, othwise nil is
returned."
  (if (file-exists-p (concat path "/" filename))
      path
    (let ((parent-dir (file-name-directory (directory-file-name path))))
      ;; Make sure we do not go into infinite recursion
      (if (string-equal path parent-dir)
          nil
        (find-parent-with-file parent-dir filename)))))

;; Search recursively for a TAGS file and set it as the one to use for
;; the specific buffer
(defun set-buffer-tags-file-name ()
  (progn
    (make-local-variable 'tags-file-name)
    (setq tags-file-name
          (find-parent-with-file default-directory "TAGS"))))

(add-hook 'find-file-hook 'set-buffer-tags-file-name)

(defun find-buildout-root (path)
  "Search PATH for a buildout root.

If a buildout root is found return the path, othwise return
nil."
  ;; find the most top one, not the first one
  (let* ((dir default-directory)
         (previous dir))
    (while (not (equalp dir nil))
      (setq dir (find-parent-with-file dir "bootstrap.py"))
      (if (not (equalp dir nil))
          (progn
            (setq previous dir)
            ;; get parent dir
            (setq dir (file-name-directory (directory-file-name dir))))))
    (message (concat "Found buildout at: " previous))
    previous))

(defun find-buildout-root-next (path)
  "Search PATH for a buildout root.

If a buildout root is found return the path, othwise return
nil."
  (find-parent-with-file default-directory "bootstrap.py"))

;; Run buildout
(defun run-buildout-based-on-buffer ()
  "Execute buildout based on the path of the current buffer. It
will search until it finds a suitable buildout to execute."
  (interactive)
  (let ((buildout-directory (find-buildout-root default-directory)))
    (if (not (eq buildout-directory nil))
        (directory-excursion
         (find-buildout-root default-directory)
         (let ((buildout (concat buildout-directory "bin/buildout")))
           ;; If bin/buildout exists just execute it
           (if (file-exists-p buildout)
               (progn
                 (set-buffer (buffer-name (compile buildout)))
                 (ignore-errors (kill-buffer "*Buildout*"))
                 (rename-buffer "*Buildout*"))
             ;; Else run bootstrap.py first by prompting the user for
             ;; a Python version to use. When bootstrap.py completes
             ;; run bin/buildout
             (let ((python-command
                    (read-string "Python command: "
                                 nil nil "python")))
               (progn (message "Running bootstrap")
                      (run-command (concat python-command " bootstrap.py")
                                   "*Buildout*")
                      (run-buildout-based-on-buffer))))))
      (message "No buildout directory found anywhere in path: %s"
               default-directory))))

;; Run the application
(defun run-application-based-on-buffer ()
  "Start the application based on the path of the current buffer."
  (interactive)
  (let ((buildout-directory (find-buildout-root default-directory)))
    (let ((instance-command (concat buildout-directory "bin/instance")))
      (cond ((file-exists-p instance-command)
             (pdb (concat instance-command " fg")))
            ((let ((instance-command (concat buildout-directory "bin/instance1")))
               (cond ((file-exists-p instance-command)
                      (pdb (concat instance-command " fg")))
                     ((let ((instance-command (concat buildout-directory "bin/instanceadm")))
                        (cond ((file-exists-p instance-command)
                               (pdb (concat instance-command " fg")))
                              ((let ((instance-command (concat buildout-directory "bin/freshen")))
                                 (cond ((file-exists-p instance-command)
                                        (run-freshen instance-command))
                                       (t (message "No application found in the buildout")))))))))))))))

(defun buildout-debug-instance ()
  "Start the application in debug mode based on the path of the current buffer."
  (interactive)
  (let ((buildout-directory (find-buildout-root default-directory)))
    (let ((instance-command (concat buildout-directory "bin/instance")))
      (cond ((file-exists-p instance-command)
             (pdb (concat instance-command " debug")))
            ((let ((instance-command (concat buildout-directory "bin/instance1")))
               (cond ((file-exists-p instance-command)
                      (pdb (concat instance-command " debug")))
                     ((let ((instance-command (concat buildout-directory "bin/instanceadm")))
                        (cond ((file-exists-p instance-command)
                               (pdb (concat instance-command " debug")))
                              ((let ((instance-command (concat buildout-directory "bin/freshen")))
                                 (cond ((file-exists-p instance-command)
                                        (run-freshen instance-command))
                                       (t (message "No application found in the buildout")))))))))))))))

(defun run-freshen (command)
  (interactive)
  ;; (interactive "MOptions [--tags dev]")
  (compile command))

(defun run-tests-based-on-buffer ()
  "Run the compile command based on the path of the current buffer"
  (interactive)
  (let ((buildout-directory (find-buildout-root-next default-directory)))
    (let ((test-command (concat buildout-directory "bin/test")))
      (cond ((file-exists-p test-command)
             (progn
               (set-buffer (buffer-name (pdb test-command)))
               (ignore-errors (kill-buffer "*Test output*"))
               (rename-buffer "*Test output*")))
            (t (message "No test command found in the buildout"))))))

(setq create-tags-command "find . -name '*.py' | etags -")
(defun run-etags-based-on-buffer ()
  "Run the etags command to create TAGS file based on the path of
the current buffer"
  (interactive)
  (directory-excursion
   (find-buildout-root default-directory)
   (set-buffer
    (buffer-name
     (compile (format create-tags-command default-directory))))
   (ignore-errors (kill-buffer "*Etags*"))
   (rename-buffer "*Etags*")
   ))

