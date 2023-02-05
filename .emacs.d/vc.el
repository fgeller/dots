(add-hook 'vc-annotate-mode-hook
	  (lambda () (setq show-trailing-whitespace nil)))

(setq vc-follow-symlinks t)

(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "--date=short" "-C" "-C" rev "--" name)))

(setq git-commit-summary-max-length 72)

(setq diff-font-lock-prettify nil)
(setq diff-font-lock-syntax nil)

(install 'git-link)

(install 'diff-hl)
(global-diff-hl-mode)
(diff-hl-margin-mode +1)
(setq diff-hl-margin-symbols-alist
	  '((insert . " ")
		(delete . " ")
		(change . " ")
		(unknown . " ")
		(ignored . " ")))

(defun fg/vc-dir-project ()
  (interactive)
  (let ((root (fg/guess-project-directory)))
	(vc-dir root)))
