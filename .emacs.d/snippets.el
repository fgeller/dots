(install 'yasnippet)
(setq yas-snippet-dir '("~/.emacs.d/snippets/"))

(install 'yasnippet-snippets)

(after 'yasnippet
  (yas-reload-all))
