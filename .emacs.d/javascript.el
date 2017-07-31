(install 'rjsx-mode)

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-hook 'js2-mode-hook (lambda () (font-lock-mode 1)))
(add-hook 'rjsx-mode-hook (lambda () (font-lock-mode 1)))
(setq js2-basic-offset 2)
