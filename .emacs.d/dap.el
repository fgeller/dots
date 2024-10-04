(use-package dap-mode 
  :ensure t 
  :commands (dap-mode)
  :config
  (require 'dap-dlv-go)
  (setq dap-auto-configure-features '(sessions locals controls))
  (setq dap-dlv-go-delve-path (expand-file-name "~/bin/dlv")))
