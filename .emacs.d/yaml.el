(use-package yaml-ts-mode
  :mode ("\\.yaml\\'" . yaml-ts-mode)
  :config 

  (defun fg/yaml-mode-hook ()
	(eldoc-mode 1)
	(yas-minor-mode)
	(setq indent-tabs-mode nil)
	(lsp-mode))
  
  (add-hook 'yaml-ts-mode-hook 'fg/yaml-mode-hook)

)
