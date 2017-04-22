(add-hook 'vc-annotate-mode-hook
	  (lambda () (setq show-trailing-whitespace nil)))

(setq vc-follow-symlinks t)

(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "--date=short" "-C" "-C" rev "--" name)))

(install 'magit)
(setq git-commit-summary-max-length 72)

(install 'git-gutter)
(setq
   git-gutter:window-width 1
   git-gutter:modified-sign " "
   git-gutter:added-sign " "
   git-gutter:deleted-sign " ")
(global-git-gutter-mode)

(install 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
