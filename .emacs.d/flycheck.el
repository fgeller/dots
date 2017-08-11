(install 'flycheck)

;; https://github.com/syl20bnr/spacemacs/tree/master/layers/+frameworks/react
(defun flycheck-set-eslint-executable ()
  (require 'flycheck)
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (global-eslint (executable-find "eslint"))
         (local-eslint (expand-file-name "node_modules/.bin/eslint" root))
         (eslint (if (file-executable-p local-eslint) local-eslint global-eslint)))
    (setq-local flycheck-javascript-eslint-executable eslint)))

(after 'flycheck
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode))


(add-hook 'rjsx-mode-hook 'flycheck-set-eslint-executable)

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

