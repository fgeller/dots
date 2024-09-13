(defun fg/emacs-lisp-mode-hook ()
  (eldoc-mode 1)
  (yas-minor-mode))

(add-hook 'emacs-lisp-mode-hook 'fg/emacs-lisp-mode-hook)
