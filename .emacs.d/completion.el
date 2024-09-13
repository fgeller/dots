(use-package corfu
  :ensure t
  :hook
  (prog-mode . corfu-mode)
  :commands
  (global-corfu-mode)
  :config
  (unless (display-graphic-p)
	(require 'popon)
	(require 'corfu-terminal)
	(corfu-terminal-mode +1))
  (setq
   corfu-cycle t
   corfu-auto t
   corfu-auto-delay 0.2
   corfu-auto-prefix 1
   corfu-quit-no-match 'separator
   corfu-on-exact-match 'quit
   corfu-preview-current nil
   )
  (define-key corfu-map (kbd "<tab>") 'corfu-insert) ;; -complete
  (define-key corfu-map (kbd "<return>") nil)
  (define-key corfu-map (kbd "RET") nil)
)

(use-package consult
  :ensure t
  :commands (
			 consult-lsp-symbols 
			 consult-lsp-diagnostics 
			 consult-flymake 
			 consult-lsp-symbols 
			 consult-lsp-file-symbols
			 consult-grep
			 consult-git-grep
			 consult-ripgrep
			 consult-yank-from-kill-ring
			 consult-line
			 consult-buffer
			 )
)

(use-package marginalia
  :ensure t
  :defer 1
  :config
  (setq marginalia-annotator-registry
		(assq-delete-all 'file marginalia-annotator-registry))
  (marginalia-mode)
)

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
)
  
(use-package embark-consult
  :ensure t 
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  :config
  (setq consult-project-function (lambda (ignore) (locate-dominating-file "." ".git")))
  )

;(autoload 'embark-consult-export-grep "embark-consult")

(after 'embark
  (add-to-list 'embark-exporters-alist '(consult-grep . embark-consult-export-grep))
  (add-to-list 'embark-exporters-alist '(consult-git-grep . embark-consult-export-grep))
  (add-to-list 'embark-exporters-alist '(consult-ripgrep . embark-consult-export-grep)))

(recentf-mode)
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)
(setq xref-prompt-for-identifier nil)

(use-package vertico
  :ensure t
  :init
  (setq-default vertico-group-format
				(concat #(" %s " 0 4 (face vertico-group-title))
						#(" ")))
  (vertico-mode))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles partial-completion)))))

(defun fg/zap-back-till-/ ()
  (interactive)
  (zap-up-to-char -1 ?/))

(define-key minibuffer-local-map (kbd "C-\\") 'fg/zap-back-till-/)

(setq enable-recursive-minibuffers t) 

(defconst fg/consult--source-git-ls-files
  `(:name     "Git ls-files"
    :narrow   (?g . "Git ls-files")
    :category file
    :face     consult-file
    :history  file-name-history
    :state    ,#'consult--file-state
    :enabled  ,(lambda () (and consult-project-function))
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
    :enabled  ,(lambda () (and consult-project-function))
    :items
    ,(lambda ()
       (let* (ps)
	 (dolist (dir '("~/src/github.com/fgeller"))
	   (let* ((p (expand-file-name dir))
		  (cmd (format "ls -d %s/*/" p))
		  (ds (split-string (shell-command-to-string cmd) "\n" t)))
	     (setq ps (append ps ds))))
	 ps)))
  "Project roots candidate source for `consult-buffer'.")

(after 'consult
  (add-to-list 'consult-buffer-sources fg/consult--source-git-ls-files t)
  (add-to-list 'consult-buffer-sources fg/consult--source-projects t))
(setq consult-preview-key "C-t")

;; https://emacs.stackexchange.com/a/30558
(defun fg/minibuffer-yank-word ()
  "Yank word at point in the buffer when entering text into minibuffer."
  (interactive)
  (with-selected-window (minibuffer-selected-window)
    (when-let ((word (current-word)))
      (with-selected-window (active-minibuffer-window)
        (insert word)))))

(define-key minibuffer-local-map "\C-w" #'fg/minibuffer-yank-word)
