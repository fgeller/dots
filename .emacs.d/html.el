(use-package mhtml-mode
  :mode
  ("\\.html\\'" . mhtml-mode)
  :config
  (defun fg/mhtml-mode-hook ()
	(yas-minor-mode)
	(indent-tabs-mode t)
	(setq tab-width 2)
	(setq css-indent-offset 2))

  (add-hook 'mhtml-mode-hook 'fg/mhtml-mode-hook))

