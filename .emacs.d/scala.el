(install 'sbt-mode)
(install 'scala-mode)
(add-hook 'scala-mode-hook 'scala-customizations)

(defun scala-customizations ()
  (setq tab-width 2)
  (subword-mode 1)
  (lsp-deferred))

(defun scala-ignore-all-tests ()
  (interactive)
  (save-excursion
    (replace-regexp "\\bit(\\(s\\)?\"" "ignore(\\1\"" nil (point-min) (point-max)))
  (unless current-prefix-arg
    (save-excursion
      (search-backward "ignore(" nil)
      (replace-match "it(" nil t))))

(defun scala-enable-all-tests ()
  (interactive)
  (save-excursion
    (replace-regexp "\\bignore(\\(s\\)?\"" "it(\\1\"" nil (point-min) (point-max))))
