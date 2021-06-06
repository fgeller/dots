(install 'lsp-mode)
(install 'lsp-ui)

(setq
 lsp-ui-sideline-show-hover nil
 lsp-ui-doc-show-with-cursor nil 
 lsp-ui-doc-position 'at-point
 lsp-modeline-diagnostics-scope :workspace
 lsp-headerline-breadcrumb-enable nil
 lsp-prefer-capf t
 lsp-file-watch-threshold 500)
