(use-package org
  :commands (org-store-link org-agenda)
  :mode ("\\.org\\'" . org-mode)
  :config
  (setq 
   calendar-week-start-day 1
   org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5)
   org-agenda-custom-commands`(("p" "[p]ersonal inbox" tags "+inbox+personal")
							   ("w" "[w]ork inbox" tags "+inbox+work")
							   ("n" "Find a TAGged note" tags "" ((org-agenda-archives-mode t))))
   org-agenda-files (list (concat org-directory "/dump.org"))
   org-agenda-include-diary t
   org-agenda-remove-tags nil
   org-agenda-sorting-strategy '((agenda habit-down time-up todo-state-down))
   org-agenda-span 'day
   org-agenda-start-on-weekday nil
   org-agenda-sticky t
   org-agenda-tags-column -125
   org-agenda-window-setup 'current-window
   org-babel-load-languages '((emacs-lisp . t))
   org-capture-templates`(("n" "note"entry (file ,org-default-notes-file)"* %?\n\n" :prepend t)
						  ("t" "task"entry (file ,org-default-tasks-file)"* TODO %?\n\n" :prepend t)
						  ("w" "work task"entry (file ,org-default-work-file)"* TODO %? \n\n")
						  ("b" "Bookmark"entry (file+headline ,(expand-file-name "bm.org" org-directory) "Bookmarks")"* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1))
   org-clock-in-resume t
   org-clock-in-switch-to-state "STARTED"
   org-clock-into-drawer t
   org-clock-out-remove-zero-time-clocks t
   org-clock-persist 'history
   org-clock-persist t
   org-default-notes-file (concat org-directory "/Notes.org")
   org-default-tasks-file (concat org-directory "/dump.org")
   org-default-work-file (concat org-directory "/work.org")
   org-directory (expand-file-name "~/orgs")
   org-duration-format 'h:mm
   org-edit-timestamp-down-means-later t
   org-enforce-todo-checkbox-dependencies t
   org-footnote-auto-label 'plain
   org-hide-emphasis-markers t
   org-hide-leading-stars t
   org-log-done 'time
   org-loop-over-headlines-in-active-region t
   org-outline-path-complete-in-steps nil
   org-refile-targets  '((nil :maxlevel . 5) 
						 (org-agenda-files :maxlevel . 5))
   org-refile-use-outline-path 'file
   org-return-follows-link t
   org-special-ctrl-a t
   org-special-ctrl-k t
   org-src-fontify-natively nil
   org-src-window-setup 'current-window
   org-startup-folded 'showall
   org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
   org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)") 
					   (sequence "WAITING(w@/!)" "|" "SUPERSEDED(u!/!)" "CANCELLED(c@/!)"))
  )
 
  (defalias 'calendar-absolute-from-iso 'calendar-iso-to-absolute)
  ;; (defadvice org-agenda (around org-agenda-fullscreen activate)
  ;; 	(window-configuration-to-register :org-agenda-fullscreen)
  ;; 	ad-do-it
  ;; 	(delete-other-windows))
  ;; (defadvice org-agenda-quit (around org-agenda-quit-fullscreen activate)
  ;; 	ad-do-it
  ;; 	(jump-to-register :org-agenda-fullscreen))

  (defun fg/org-mode-hook ()
	(font-lock-mode 1)
	(org-hide-block-all)
	(yas-minor-mode)
	(turn-on-auto-fill))
  (add-hook 'org-mode-hook 'fg/org-mode-hook)

  (org-clock-persistence-insinuate)

  (require 'org-crypt)
  (require 'org-tempo) ;; <s for quick templates

  (defun schedule-task-now ()
	(interactive)
	(let ((now (with-temp-buffer (org-time-stamp '(16)) (buffer-string))))
      (org-schedule nil now)
      (message "Scheduled started task for now")))

  (add-hook 'org-clock-in-hook 'schedule-task-now)
  )
