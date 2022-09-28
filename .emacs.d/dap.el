(install 'dap-mode)
(install 'dap-dlv-go 'require)

(setq dap-dlv-go-delve-path (expand-file-name "~/bin/dlv"))
(dap-mode 1)
(dap-ui-mode 1)
(dap-tooltip-mode 1)
(tooltip-mode 1)
(dap-ui-controls-mode 1)

;; (setq dap-print-io t)
;; https://emacs-lsp.github.io/dap-mode/page/configuration/#go
;; https://github.com/emacs-lsp/dap-mode/commit/6b2cca8fd5aa3ae21a3b9143ec987c487f9e913d#commitcomment-84428823
