(defun fg/sql-customizations ()
  (message "starting sql customizations")
  (setq tab-width 2)
  (setq indent-tabs-mode nil)
  (message "finished sql customizations"))

(add-hook 'sql-mode-hook 'fg/sql-customizations)
