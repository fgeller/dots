(install 'flycheck)

(after 'go-mode
  (require 'flycheck)
  (flycheck-define-checker go-unused
    ""
    :command ("unused" ".")
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": " (message) line-end))
    :modes go-mode
    :predicate flycheck-buffer-saved-p)
  (flycheck-define-checker go-gosimple
    ""
    :command ("gosimple" ".")
    :error-patterns
    ((warning line-start (file-name) ":" line ":" column ": " (message) line-end))
    :modes go-mode
    :predicate flycheck-buffer-saved-p)

  (add-to-list 'flycheck-checkers 'go-gosimple)
  (add-to-list 'flycheck-checkers 'go-unused)
  (flycheck-add-next-checker 'go-test 'go-gosimple 'append)
  (flycheck-add-next-checker 'go-test 'go-unused 'append))

