(install 'wgrep)

(setq isearch-lazy-count t)

(install 'ag)

(defun list-todo-in-current-dir ()
  (interactive)
  (ag-regexp "(TODO|FIXME)" default-directory))
