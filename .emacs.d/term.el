(use-package eat
  :ensure 
  (
  :type git
  :host codeberg
  :repo "akib/emacs-eat"
  :files ("*.el" ("term" "term/*.el") "*.texi"
          "*.ti" ("terminfo/e" "terminfo/e/*")
          ("terminfo/65" "terminfo/65/*")
          ("integration" "integration/*")
          (:exclude ".dir-locals.el" "*-tests.el"))) 
  :commands (eat)
  :config
  (define-key eat-mode-map (kbd "<escape>") 'modal-mode-activate))


