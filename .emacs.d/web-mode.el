(install 'web-mode)
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
(setq web-mode-enable-auto-closing nil)
(add-hook 'web-mode-hook 'web-mode-customizations)

(defun web-mode-customizations ()
  (yas-minor-mode 1))


