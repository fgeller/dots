(install 'lsp-mode)
(install 'lsp-ui)
(install 'company-lsp)

(setq
 lsp-session-file "~/.emacs.d/transient/lsp-session-v1"
 lsp-eldoc-render-all t
 lsp-ui-doc-enable nil
 lsp-ui-sideline-enable nil
 lsp-prefer-capf t
 lsp-idle-delay 0.1)
