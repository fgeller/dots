(add-hook 'vc-annotate-mode-hook
	  (lambda () (setq show-trailing-whitespace nil)))

(setq vc-follow-symlinks t)

(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 'async nil "blame" "--date=short" "-C" "-C" rev "--" name)))

(setq git-commit-summary-max-length 72)

(install 'magit)
(install 'forge)
(setq magit-auto-revert-mode nil)
(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
(setq magit-refresh-status-buffer nil)

(install 'diff-hl)
(global-diff-hl-mode)
(diff-hl-margin-mode +1)

(setq diff-hl-margin-symbols-alist '((insert . " ")
				     (delete . " ")
				     (change . " ")
				     (unknown . " ")
				     (ignored . " ")))
