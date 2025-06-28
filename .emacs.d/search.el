(use-package wgrep 
  :ensure t 
  :commands (wgrep-setup wgrep-rg))

(use-package transient 
  :ensure (:repo "https://github.com/magit/transient.git" :branch "main"))

(use-package rg 
  :ensure t
  :commands (rg)
  :config
  (require wgrep-rg)
  (rg-enable-default-bindings))

(setq isearch-lazy-count t)

