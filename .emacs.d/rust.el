(use-package rust-ts-mode
  :mode ("\\.rs\\'" . rust-ts-mode)
  :config

  (defun fg/rust-mode-hook ()
	(eldoc-mode 1)
	(subword-mode 1)
	(yas-minor-mode)
	(setq tab-width 4)
	(eglot-ensure)
	(apheleia-mode +1)
	(add-hook 'before-save-hook  (lambda () (call-interactively 'eglot-code-action-organize-imports)) nil t)
	)
  
  (add-hook 'rust-ts-mode-hook 'fg/rust-mode-hook))
