(install 'ag)
(install 'wgrep)
(install 'wgrep-ag)

(after 'ag
  (setq ag-group-matches nil)
  (add-to-list 'ag-ignore-list "vendor" t)
  (add-to-list 'ag-ignore-list "target" t))

(defun ag-project-with-thing-at-point ()
  (interactive)
  (let ((thing (thing-at-point 'symbol)))
    (ag-project thing)))

(defun ag-with-thing-at-point ()
  (interactive)
  (let ((thing (thing-at-point 'symbol)))
    (ag thing default-directory)))
