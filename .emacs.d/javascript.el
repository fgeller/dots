(install 'prettier-js)

;; follows gts setup
;; https://github.com/google/gts/blob/master/.prettierrc.json
(setq prettier-js-args
      '(
	"--trailing-comma" "es5"
	"--bracket-spacing" "false"
	"--single-quote" "true"
	"--arrow-parens" "avoid"
	))

(defun fg/js ()
  (lsp-deferred)
  (subword-mode 1)
  (prettier-js-mode 1)
  (setq-local tab-width 2)
  (setq-local indent-tabs-mode nil)
  (setq-local typescript-indent-level 2)
  (setq-local js-indent-level 2))

(add-hook 'js-mode-hook 'fg/js)

(install 'typescript-mode)
(add-hook 'typescript-mode-hook 'fg/js)
