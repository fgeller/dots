(setq org-startup-folded 'showall)

(autoload 'org-store-link "ol" "Org Mode." t)
(setq org-log-done 'time)

(setq
 org-special-ctrl-k t
 org-special-ctrl-a t)

;; <s for quick templates
(require 'org-tempo)

(setq org-return-follows-link t)

(setq calendar-week-start-day 1)

(defalias 'calendar-absolute-from-iso 'calendar-iso-to-absolute)
(setq org-duration-format 'h:mm)

(setq
 org-hide-emphasis-markers t
 org-hide-leading-stars t)

(setq org-edit-timestamp-down-means-later t)

(setq org-footnote-auto-label 'plain)

(defun fg/org-mode-customization ()
  (yas-minor-mode)
  (turn-on-auto-fill))

(add-hook 'org-mode-hook 'fg/org-mode-customization)

(setq org-enforce-todo-checkbox-dependencies t)

(setq org-loop-over-headlines-in-active-region t)

(setq org-agenda-sticky t)

(add-hook 'org-mode-hook #'(lambda () (interactive) (font-lock-mode 1)))
(add-hook 'org-mode-hook 'org-hide-block-all)

(setq
 org-directory (expand-file-name "~/orgs")
 org-default-tasks-file (concat org-directory "/dump.org")
 org-default-work-file (concat org-directory "/work.org")
 org-default-notes-file (concat org-directory "/Notes.org")
 org-agenda-files (list (concat org-directory "/dump.org")))

(after 'org
  (setq org-clock-persist 'history)
  (setq org-time-clocksum-format (quote (:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)))
  (org-clock-persistence-insinuate)
  (dolist (org-mod '(org-crypt))
    (require org-mod)))

(setq org-todo-keywords '(
                    (sequence "TODO(t)" "STARTED(s)" "|" "DONE(d!/!)")
                    (sequence "WAITING(w@/!)" "|" "SUPERSEDED(u!/!)" "CANCELLED(c@/!)")))

(setq org-agenda-include-diary t)

(setq org-agenda-span 'day)

(setq org-agenda-remove-tags nil)

(setq org-agenda-tags-column -125)

(setq org-agenda-start-on-weekday nil)

(setq org-agenda-window-setup 'current-window)

(setq org-agenda-custom-commands
      `(;; match those tagged with :inbox:, are not scheduled, are not DONE.
        ("p" "[p]ersonal inbox" tags "+inbox+personal")
        ("w" "[w]ork inbox" tags "+inbox+work")
        ("n" "Find a TAGged note" tags "" ((org-agenda-archives-mode t)))))

(setq org-agenda-sorting-strategy '((agenda habit-down time-up todo-state-down)))

(defadvice org-agenda (around org-agenda-fullscreen activate)
  (window-configuration-to-register :org-agenda-fullscreen)
  ad-do-it
  (delete-other-windows))

(defadvice org-agenda-quit (around org-agenda-quit-fullscreen activate)
  ad-do-it
  (jump-to-register :org-agenda-fullscreen))

(setq org-clock-persist t)

(setq org-clock-in-resume t)

(setq org-clock-in-switch-to-state "STARTED")

(setq org-clock-into-drawer t)

(setq org-clock-out-remove-zero-time-clocks t)

(defun schedule-task-now ()
  (interactive)
  (let ((now (with-temp-buffer (org-time-stamp '(16)) (buffer-string))))
    (org-schedule nil now)
    (message "Scheduled started task for now")))

(add-hook 'org-clock-in-hook 'schedule-task-now)

(setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 5))

(setq org-capture-templates
      `(("n" "note"
         entry (file ,org-default-notes-file)
         "* %?\n\n" :prepend t)
        ("t" "task"
         entry (file ,org-default-tasks-file)
         "* TODO %?\n\n" :prepend t)
        ("w" "work task"
         entry (file ,org-default-work-file)
         "* TODO %? \n\n")
        ("b" "Bookmark"
         entry (file+headline ,(expand-file-name "bm.org" org-directory) "Bookmarks")
		 "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)))

(setq
 org-outline-path-complete-in-steps nil
 org-refile-use-outline-path 'file
 org-refile-targets  '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))

(setq org-src-window-setup 'current-window)

(setq org-src-fontify-natively nil)

(setq org-babel-load-languages '((emacs-lisp . t)))
