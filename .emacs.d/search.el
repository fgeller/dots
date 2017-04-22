(install 'ag)
(install 'wgrep)
(install 'wgrep-ag)

(defun ag-project-with-thing-at-point ()
  (interactive)
  (let ((thing (thing-at-point 'symbol)))
    (ag-project thing)))

(defun ag-with-thing-at-point ()
  (interactive)
  (let ((thing (thing-at-point 'symbol)))
    (ag thing default-directory)))
