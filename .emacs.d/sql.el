(install 'sqlformat 'require)
(after 'sqlformat (setq sqlformat-command 'pgformatter))

(defun fg/sql-customizations ()
  (setq tab-width 2)
  (setq indent-tabs-mode nil))

(add-hook 'sql-mode-hook 'fg/sql-customizations)
