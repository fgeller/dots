(use-package wgrep :commands (wgrep-setup wgrep-rg))
(use-package rg 
  :commands (rg)
  :config
  (require wgrep-rg)
  (rg-enable-default-bindings)
)

(setq isearch-lazy-count t)

