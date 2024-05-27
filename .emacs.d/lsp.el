(install 'lsp-mode)
(install 'lsp-ui)
(install 'lsp-marksman)
(install 'lsp-pyright 'require)

(setq
 lsp-ui-sideline-show-hover nil
 lsp-ui-doc-show-with-cursor nil
 lsp-ui-sideline-show-symbol nil
 lsp-ui-sideline-show-code-actions nil
 lsp-ui-doc-position 'at-point
 lsp-ui-doc-enable nil
 lsp-ui-sideline-enable nil
 lsp-modeline-code-actions-enable nil
 lsp-diagnostics-provider :flycheck
 lsp-modeline-diagnostics-scope :file
 lsp-headerline-breadcrumb-enable nil
 lsp-headerline-breadcrumb-icons-enable nil
 lsp-eldoc-render-all nil
 lsp-completion-provider :none
 lsp-lens-enable nil
 lsp-signature-auto-activate nil
 lsp-idle-delay 1.0
 ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
 lsp-enable-symbol-highlighting nil
 lsp-enable-file-watchers nil 
 lsp-file-watch-threshold 5000
 lsp-log-io nil
 lsp-client-packages '(lsp-cmake lsp-dockerfile lsp-go lsp-javascript lsp-json lsp-markdown lsp-terraform lsp-xml lsp-yaml)
 lsp-restart 'ignore
)

(defun fg/lsp-customizations ()
  (setf (alist-get 'styles
		   (alist-get 'lsp-capf completion-category-defaults))
	'(orderless)))

(add-hook 'lsp-after-open-hook 'fg/lsp-customizations)
