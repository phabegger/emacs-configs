;;;; Full-ack
(add-to-list 'load-path (concat vendor-dir "/full-ack.el"))
(require 'full-ack)

(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)
