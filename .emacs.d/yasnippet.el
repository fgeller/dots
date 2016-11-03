(require-package 'yasnippet)
(setq yas-snippet-dirs (expand-file-name "~/.emacs.d/snippets"))

(after 'yasnippet
  (setq-default yas-prompt-functions (delete 'yas-x-prompt yas-prompt-functions))
  (yas-reload-all))
