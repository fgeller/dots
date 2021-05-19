(install 'lsp-mode)
(install 'lsp-ui)
;; (install 'company-lsp)

(setq
 lsp-session-file "~/.emacs.d/transient/lsp-session-v1"
 lsp-eldoc-render-all nil
 lsp-modeline-diagnostics-scope :file
 lsp-file-watch-threshold 500
 lsp-enable-file-watchers nil
 lsp-ui-doc-enable nil
 lsp-ui-doc-delay 0.75
 lsp-ui-doc-max-height 200
 lsp-ui-sideline-enable nil
 lsp-enable-folding nil
 lsp-enable-text-document-color nil
 lsp-enable-on-type-formatting nil
 ;; lsp-ui-sideline-show-hover nil
 ;; lsp-ui-sideline-show-code-actions nil
 ;; lsp-ui-sideline-show-diagnostics nil
 lsp-headerline-breadcrumb-enable nil
 lsp-prefer-capf t
 lsp-idle-delay 1)
