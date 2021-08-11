(install 'company)
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0)
(global-company-mode)

(install 'consult)
(install 'consult-flycheck)
(install 'consult-lsp)

(install 'embark)
(install 'embark-consult)
(autoload 'embark-consult-export-grep "embark-consult")

(after 'embark
  (add-to-list 'embark-exporters-alist '(consult-grep . embark-consult-export-grep))
  (add-to-list 'embark-exporters-alist '(consult-git-grep . embark-consult-export-grep))
  (add-to-list 'embark-exporters-alist '(consult-ripgrep . embark-consult-export-grep)))

(install 'marginalia)
;;(marginalia-mode -1) ;; i really don't want file permission in the minibuffer and it seems slower?

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

(defconst fg/consult--source-git-ls-files
  `(:name     "Git ls-files"
    :narrow   (?g . "Git ls-files")
    :category file
    :face     consult-file
    :history  file-name-history
    :state    ,#'consult--file-state
    :enabled  ,(lambda () (and consult-project-root-function))
    :items
    ,(lambda ()
      (when-let (root (consult--project-root))
	(let* ((default-directory root)
	       (cmd (format "git ls-files --full-name"))
	       (files (split-string (shell-command-to-string cmd) "\n" t))
	       (abs-files (mapcar (lambda (fn) (expand-file-name fn root)) files))
	       )
	  abs-files))))
  "Git ls-files candidate source for `consult-buffer'.")

(defconst fg/consult--source-projects
  `(:name     "Project Roots"
    :narrow   (?p . "Project Roots")
    :category file
    :face     consult-file
    :history  file-name-history
    :state    ,#'consult--file-state
    :enabled  ,(lambda () (and consult-project-root-function))
    :items
    ,(lambda ()
       (let* (ps)
	 (dolist (dir '("~/src/github.com/fgeller" "~/src/gitlab.com/refurbed"))
	   (let* ((p (expand-file-name dir))
		  (cmd (format "ls -d %s/*/" p))
		  (ds (split-string (shell-command-to-string cmd) "\n" t)))
	     (setq ps (append ps ds))))
	 ps)))
  "Project roots candidate source for `consult-buffer'.")

(after 'consult
  (add-to-list 'consult-buffer-sources fg/consult--source-git-ls-files)
  (add-to-list 'consult-buffer-sources fg/consult--source-projects)

 (consult-customize
  consult-ripgrep consult-git-grep consult-grep
  consult-bookmark consult-recent-file consult-xref
  consult--source-file consult--source-project-file consult--source-bookmark
  fg/consult--source-git-ls-files
  fg/consult--source-projects
  :preview-key (kbd "C-M-."))
  )

