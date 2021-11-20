(install 'yasnippet)
(setq yas-snippet-dir '("~/.emacs.d/snippets/"))

(install 'yasnippet-snippets)

(yas-global-mode 1)

(after 'yasnippet
  (yas-reload-all))

