(install 'prettier-js)

(defun fg/ts ()
  (lsp-deferred)
  (subword-mode 1)
  (prettier-js-mode 1)
  (setq-local tab-width 2)
  (setq-local typescript-indent-level 2)
  (setq-local js-indent-level 2)
  (setq indent-tabs-mode nil))

(install 'typescript-mode)
(add-hook 'typescript-mode-hook 'fg/ts)

(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . typescript-mode))
