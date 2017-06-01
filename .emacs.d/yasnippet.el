(install 'yasnippet)
(setq yas-snippet-dirs (list (expand-file-name "~/.emacs.d/snippets")))

(after 'yasnippet
  (setq-default yas-prompt-functions (delete 'yas-x-prompt yas-prompt-functions))
  (yas-reload-all))
