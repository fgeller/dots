(use-package wgrep 
  :ensure t 
  :commands (wgrep-setup wgrep-rg))
(use-package rg 
  :ensure t
  :commands (rg)
  :config
  (require wgrep-rg)
  (rg-enable-default-bindings))

(setq isearch-lazy-count t)

