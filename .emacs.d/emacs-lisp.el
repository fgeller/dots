(defun fg/emacs-lisp-mode-hook ()
  (eldoc-mode 1)
  (setq tab-width 8)
  (yas-minor-mode))

(add-hook 'emacs-lisp-mode-hook 'fg/emacs-lisp-mode-hook)
