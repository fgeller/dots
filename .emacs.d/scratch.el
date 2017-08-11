(defvar persistent-scratch-filename "~/orgs/scratch")

(defun save-persistent-scratch ()
  (with-current-buffer (get-buffer "scratch")
    (basic-save-buffer)))

(defun load-persistent-scratch ()
  (find-file persistent-scratch-filename)
  (when (get-buffer "*scratch*") (kill-buffer "*scratch*")))

(load-persistent-scratch)
(push #'save-persistent-scratch kill-emacs-hook)
