(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")
(require 'mu4e)

(setq
 user-full-name "Felix Geller"
 user-mail-address "fgeller@protonmail.com"
 
 message-send-mail-function 'smtpmail-send-it
 message-kill-buffer-on-exit t
 
 smtpmail-stream-type 'starttls
 smtpmail-smtp-server "127.0.0.1"
 smtpmail-smtp-service 1026
 smtpmail-smtp-user "fgeller@protonmail.com"
 smtpmail-queue-mail nil
 smtpmail-queue-dir (expand-file-name "~/Mail/ProtonMail/queue/cur")
 
 mu4e-maildir (expand-file-name "~/Mail")
 mu4e-get-mail-command "offlineimap"
 mu4e-sent-folder "/Sent"
 mu4e-trash-folder "/Trash"
 mu4e-drafts-folder "/Drafts"
 mu4e-refile-folder "/Archive"
 mu4e-attachment-dir (expand-file-name "~/Downloads")

 mu4e-headers-fields '((:human-date . 12)
		       (:from . 22)
		       (:subject))
 mu4e-headers-visible-lines (+ 3 (/ (frame-total-lines) 3))
 mu4e-use-fancy-chars t

 mu4e-view-use-gnus nil
 mu4e-view-show-images t
 mu4e-view-image-max-width 800
 mu4e-view-prefer-html t

 mu4e-sent-messages-behavior 'delete
) 

(defun fg/view-in-eww (msg)
  (eww-browse-url (concat "file://" (mu4e~write-body-to-html msg))))

(add-to-list 'mu4e-view-actions '("Browser" . mu4e-action-view-in-browser) t)
(add-to-list 'mu4e-view-actions '("XWidget" . mu4e-action-view-with-xwidget) t)
(add-to-list 'mu4e-view-actions '("Eww" . fg/view-in-eww) t)

;; mu init -m ~/Mail/ProtonMail --my-address fgeller@protonmail.com --my-address fgeller@pm.me --my-address fgeller@gmail.com

(when (fboundp 'imagemagick-register-types)
  (imagemagick-register-types))
