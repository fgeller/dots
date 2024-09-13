(use-package prettier-js
  :ensure t
  :commands (prettier-js-mode))

(use-package typescript-ts-mode
  :mode (("\\.tsx?\\'" . typescript-ts-mode)
		 ("\\.jsx?\\'" . typescript-ts-mode))
  
  :config
  (defun fg/typescript-mode-hook ()
	(lsp-mode 1)
	(subword-mode 1)
	(prettier-js-mode 1)
	(setq-local tab-width 2
				typescript-indent-level 2
				js-indent-level 2)
	(setq indent-tabs-mode nil))
  
  (add-hook 'typescript-ts-mode-hook 'fg/typescript-mode-hook))


(message "hele")
