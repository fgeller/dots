(install 'elisp-slime-nav)

(defun emacs-lisp-customizations ()
  (eldoc-mode 1)
  (elisp-slime-nav-mode 1))

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-customizations)
