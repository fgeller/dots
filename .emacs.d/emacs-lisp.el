(install 'elisp-slime-nav)

(defun emacs-lisp-customizations ()
  (eldoc-mode 1)
  (yas-minor-mode 1)
  (elisp-slime-nav-mode 1))

(define-key emacs-lisp-mode-map (kbd "C-M-b") 'eval-buffer)

(add-hook 'emacs-lisp-mode-hook 'emacs-lisp-customizations)
