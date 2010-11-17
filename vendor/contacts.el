(require 'json)

(defcustom contact-xtra-executable "~/.emacs.d/sbin/xtra.py"
  "Path to the xtra.py executable"
  :group 'contact
  :type 'file)

(defcustom contact-xtra-username ""
  "Username for xtra zone (used for sending sms)"
  :group 'contact
  :type 'string)

(defcustom contact-xtra-temp-directory "."
  "Temp directory for storing captcha images and session infos"
  :group 'contact
  :type 'directory)

(defun contact--list-contacts (arg)
  "List of all contacts in form: NAME <ARG>
The Argument is embedded in the 'contacts' call, see formats in 'man contacts'"
  (split-string (shell-command-to-string
                 (concat "contacts -f '%n <" arg ">' -HS | grep -v '<>'"))
                "\n" t))

(defun contact-send-email ()
  (interactive)
  (let* ((contact (ido-completing-read "Contact: " (contact--list-contacts "%e")))
         (email (nth 1 (split-string (nth 0 (split-string contact ">")) "<"))))
    (do-applescript (format "
tell application \"Mail\"
  set theMessage to make new outgoing message
  set visible of theMessage to true
  tell theMessage
      make new to recipient at end of to recipients with properties {address:\"%s\"}
  end tell
  activate
end tell
" email))))

(defun contact-send-email-with-file-as-attachment ()
  (interactive)
  (let* ((contact (ido-completing-read "Contact: " (contact--list-contacts "%e")))
         (email (nth 1 (split-string (nth 0 (split-string contact ">")) "<"))))
    (do-applescript (format "
tell application \"Mail\"
  set theMessage to make new outgoing message
  set visible of theMessage to true
  tell theMessage
      make new to recipient at end of to recipients with properties {address:\"%s\"}
      make new attachment with properties {file name:\"%s\"} at after last paragraph
  end tell
  activate
end tell
" email buffer-file-name))))

(defun contact-send-email-with-buffer-as-text ()
  (interactive)
  (let* ((contact (ido-completing-read "Contact: " (contact--list-contacts "%e")))
         (email (nth 1 (split-string (nth 0 (split-string contact ">")) "<"))))
    (do-applescript (format "
tell application \"Mail\"
  set theMessage to make new outgoing message with properties {content:\"%s\"}
  set visible of theMessage to true
  tell theMessage
      make new to recipient at end of to recipients with properties {address:\"%s\"}
      make new attachment with properties {file name:\"%s\"} at after last paragraph
  end tell
  activate
end tell
" (encode-coding-string (replace-regexp-in-string "\"" "\\\\\"" (buffer-string))
                        'iso-8859-1) email buffer-file-name))))

(defun contact-send-sms ()
  (interactive)
  (let* ((contact (ido-completing-read "Contact: " (contact--list-contacts "%mp")))
         (phone (nth 1 (split-string (nth 0 (split-string contact ">")) "<")))
         (message (read-from-minibuffer "Message: "))
         (jsondata
          (shell-command-to-string
           (concat contact-xtra-executable " login -u '" contact-xtra-username
                   "' -p '" (read-passwd (concat "Password for " contact-xtra-username ": ")) "' -d '"
                   contact-xtra-temp-directory "'")))
         (data (json-read-from-string jsondata))
         (token (cdr (assoc 'token data)))
         (captchafile (cdr (assoc 'captcha data))))
    (find-file captchafile)
    (let* ((captchatext (read-from-minibuffer "Captcha passphrase: ")))
      (shell-command (concat contact-xtra-executable " send -t '" token "' -c '"
                             captchatext "' -n '" phone "' -m '" message "' -d '"
                             contact-xtra-temp-directory "'"))
      (senny-kill-buffer))))

(provide 'contacts)
