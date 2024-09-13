(defun eshell/c (&rest args)
  (let* ((cmd (mapconcat 'identity args " ")))
	(compile cmd)))

(defun eshell/grt ()
  (let* ((root (fg/guess-project-directory)))
	(cd root)))

(use-package vterm :commands (vterm))
