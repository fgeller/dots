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

;; restore win layout after quitting ediff
;; https://emacs.stackexchange.com/a/17089
(defvar fg/ediff-last-window-layout nil)

(defun fg/ediff-wrapper ()
  (interactive)
  (fg/store-pre-ediff-window-layout)
  (call-interactively 'ediff))

(defun fg/vc-ediff-wrapper ()
  (interactive)
  (fg/store-pre-ediff-window-layout)
  (call-interactively 'vc-ediff))

(defun fg/store-pre-ediff-window-layout ()
  (message "storing ediff config")
  (setq fg/ediff-last-window-layout (current-window-configuration)))

(defun fg/restore-pre-ediff-window-layout ()
  (set-window-configuration fg/ediff-last-window-layout))

;; (add-hook 'ediff-before-setup-hook #'fg/store-pre-ediff-window-layout)
(add-hook 'ediff-quit-hook #'fg/restore-pre-ediff-window-layout)
