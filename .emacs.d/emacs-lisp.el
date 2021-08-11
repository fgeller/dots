(defun fg/emacs-lisp ()
  (eldoc-mode 1)
  (yas-minor-mode))

(add-hook 'emacs-lisp-mode-hook 'fg/emacs-lisp)
