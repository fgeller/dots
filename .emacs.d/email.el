(setq
 mail-envelope-from 'header
 mail-from-style 'angles
 mail-host-address "fury"
 mail-specify-envelope-from t
 message-kill-buffer-on-exit t
 message-send-mail-partially-limit nil
 message-sendmail-envelope-from 'header
 message-signature "Felix Geller"
 send-mail-function 'sendmail-send-it
 user-full-name "Felix Geller"
 user-mail-address "fg@m2l.io"
)

(install 'notmuch)

(defun create-mail ()
  (interactive)
  (require 'notmuch)
  (notmuch-mua-mail))

(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

(after 'shr
       (define-key shr-map (kbd "a") nil)
       (define-key shr-map (kbd "i") nil)
       (define-key shr-map (kbd "I") nil)
       (define-key shr-map (kbd "o") nil)
       (define-key shr-map (kbd "v") nil)
       (define-key shr-map (kbd "u") nil)
       (define-key shr-map (kbd "w") nil)
       (define-key shr-map (kbd "z") nil)
       (define-key shr-map (kbd "TAB") nil)
       (define-key shr-map (kbd "M-TAB") nil))

(after 'notmuch
  (setq notmuch-fcc-dirs "sent +sent"
        notmuch-crypto-process-mime t
        notmuch-show-indent-messages-width 2
        notmuch-archive-tags '("-inbox" "-spam" "-movio-in" "-m2l-in" "+archive" "-flagged")
        notmuch-saved-searches '((:name "in" :query "tag:inbox" :key "i")
                                 (:name "movio-in" :query "tag:movio-in" :key "m")
                                 (:name "m2l-in" :query "tag:m2l-in" :key "2")
                                 (:name "flagged" :query "tag:flagged" :key "f")
                                 (:name "sent" :query "tag:sent" :key "t")
                                 (:name "spam" :query "tag:spam" :key "s")
                                 (:name "drafts" :query "tag:draft" :key "d"))
        notmuch-hello-sections '(notmuch-hello-insert-saved-searches
                                 notmuch-hello-insert-search
                                 notmuch-hello-insert-recent-searches
                                 notmuch-hello-insert-alltags))
  (add-hook 'notmuch-show-hook (lambda () (setq show-trailing-whitespace nil)))
  (mapcar (lambda (key)
		 (define-key notmuch-search-mode-map (kbd key) (lookup-key fingers-mode-map (kbd key)))
		 (define-key notmuch-show-mode-map (kbd key) (lookup-key fingers-mode-map (kbd key))))
	       '("f" "u" "p"
		 "y" "n" "e" "o" "i" "'"
		 "Y" "N" "E" "O" "I" "\""
		 "k" "l" "." "/"))

       (define-key notmuch-search-mode-map (kbd "a") 'notmuch-search-archive-thread)
       (define-key notmuch-search-mode-map (kbd "t") 'notmuch-search-tag)
       (define-key notmuch-show-mode-map (kbd "F") 'notmuch-show-forward-message)
       (define-key notmuch-show-mode-map (kbd "b") 'notmuch-show-view-part)
       (define-key notmuch-show-mode-map (kbd "B") 'notmuch-open-html-part-externally)

       (add-hook 'notmuch-show-hook 'notmuch-show-prefer-html-over-text))

(defun notmuch-open-html-part-externally ()
  (interactive)
  (search-forward "[ text/html" (point-max) t)
  (forward-line 1)
  (mm-display-external (notmuch-show-current-part-handle) "open %s"))

(defun notmuch-show-prefer-html-over-text ()
  (interactive)
  (let* ((text-button (save-excursion
                        (goto-char (point-min))
                        (search-forward "[ text/plain ]" (point-max) t)))
         (html-button (save-excursion
                        (goto-char (point-min))
                        (search-forward "[ text/html (hidden) ]" (point-max) t))))
    (when html-button
      (save-excursion
        (goto-char (- html-button 1))
        (notmuch-show-toggle-part-invisibility)))
    (when text-button
      (save-excursion
        (goto-char (- text-button 1))
        (notmuch-show-toggle-part-invisibility)))))

(after 'message
       (install 'bbdb)
       (after 'bbdb
	      (bbdb-initialize 'message)
	      (bbdb-mua-auto-update-init 'message)
	      (setq bbdb-mua-auto-update-p 'query
		    bbdb-mua-pop-up nil
		    bbdb-mua-mode-alist '((vm vm-mode vm-virtual-mode vm-summary-mode vm-presentation-mode)
					  (gnus gnus-summary-mode gnus-article-mode gnus-tree-mode)
					  (rmail rmail-mode rmail-summary-mode)
					  (mh mhe-mode mhe-summary-mode mh-folder-mode)
					  (message notmuch-message-mode message-mode)
					  (mail mail-mode)
					  (mu4e mu4e-view-mode))
		    )))

(defun offlineimap ()
  "Helper to (re)start offlineimap via compile"
  (interactive)
  (let ((buf "*offline-imap*"))
    (if (get-buffer buf) (with-current-buffer buf (recompile))
      (compile "offlineimap")
      (with-current-buffer "*compilation*" (rename-buffer buf)))))
