(install 'wgrep)
(install 'rg)
(after 'rg
  (require 'wgrep-rg)
  (rg-enable-default-bindings))
(setq isearch-lazy-count t)

