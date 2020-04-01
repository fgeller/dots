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

(defun js-customizations ()
  (lsp-deferred)
  (subword-mode 1)
  (yas-minor-mode 1)
  (font-lock-mode 1)
  (prettier-js-mode 1))

(add-hook 'js-mode-hook 'js-customizations)
