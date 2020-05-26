(install 'wgrep)

(setq isearch-lazy-count t)

(defun grep-active-region ()
  (interactive)
  ;; TODO
  )

(defun list-todo-in-current-dir ()
  (interactive)
  (grep "grep -nHsI --color=never -B1 -A2 TODO *")
  (save-excursion
    (with-current-buffer "*grep*"
      (highlight-regexp "\\(TODO\\|FIXME\\)"))))

