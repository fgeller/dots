(install 'lsp-mode)
(install 'lsp-ui)

(setq
 lsp-ui-sideline-show-hover nil
 lsp-ui-doc-show-with-cursor nil 
 lsp-ui-doc-position 'at-point
 lsp-modeline-diagnostics-scope :file
 lsp-headerline-breadcrumb-enable t
 lsp-eldoc-render-all t
 lsp-prefer-capf t
 lsp-idle-delay 1.0
 lsp-enable-symbol-highlighting nil
 lsp-client-packages '(lsp-cmake lsp-dockerfile lsp-go lsp-javascript lsp-json lsp-markdown lsp-terraform lsp-xml lsp-yaml)
 lsp-file-watch-threshold 1000)

;; TODO disable unused ones?
;;   (dolist (feature '(lsp-ui-peek lsp-ui-sideline lsp-ui-doc lsp-ui-imenu))
