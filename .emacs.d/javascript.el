(install 'tern)
(install 'company-tern)

(after 'company-mode
  (add-to-list 'company-backends 'company-tern))

(install 'rjsx-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(defun rjsx-mode-customizations ()
  (setq js2-basic-offset 2)
  (font-lock-mode 1)
  (tern-mode 1))
(add-hook 'rjsx-mode-hook 'rjsx-mode-customizations)




