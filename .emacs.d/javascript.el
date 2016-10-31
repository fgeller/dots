(require-package 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook (lambda () (font-lock-mode 1)))
(setq js2-basic-offset 2)
