(install 'company)
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0)
(global-company-mode)

(install 'consult)
(recentf-mode)
(setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

(install 'vertico)
(install 'orderless)
(vertico-mode)
(setq completion-styles '(orderless)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))

(defun fg/zap-back-till-/ ()
  (interactive)
  (zap-up-to-char -1 ?/))

(define-key minibuffer-local-map (kbd "C-.") 'fg/zap-back-till-/)

(setq enable-recursive-minibuffers t) 

