(use-package web-mode :ensure web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
(setq web-mode-enable-auto-closing nil)

(defun web-mode-customizations ()
  (yas-minor-mode 1))

(add-hook 'web-mode-hook 'web-mode-customizations)
