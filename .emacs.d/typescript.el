(use-package typescript-ts-mode
  :mode (("\\.tsx?\\'" . tsx-ts-mode)
	 ("\\.jsx?\\'" . tsx-ts-mode))
  
  :config
  (defun fg/typescript-mode-hook ()
	(eglot-ensure)
	(subword-mode 1)
	(apheleia-mode +1)
	(setq-local tab-width 2
				typescript-indent-level 2
				js-indent-level 2)
	(setq indent-tabs-mode nil)
	(hs-minor-mode 1))

  (add-hook 'tsx-ts-mode-hook 'fg/typescript-mode-hook))
