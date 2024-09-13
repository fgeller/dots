(use-package yasnippet
  :commands (yas-minor-mode yas-expand)
  :defer t
  :ensure
  yasnippet-snippets
  :config
  (setq yas-snippet-dir '("~/.emacs.d/snippets/"))
  (after 'yasnippet (yas-reload-all))
)


