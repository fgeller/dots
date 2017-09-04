(install 'ensime)
(install 'sbt-mode)
(install 'scala-mode)
(add-hook 'scala-mode-hook 'scala-customizations)

(setq ensime-startup-notification nil
      ensime-startup-snapshot-notification nil)

(defun scala-customizations ()
  (setq tab-width 2)
  (font-lock-mode -1)
  (setq ensime-sem-high-enabled-p t)
  (setq ensime-eldoc-hints 'all)
  (setq ensime-typecheck-when-idle nil)
  (subword-mode 1)
  (yas-minor-mode 1))

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
