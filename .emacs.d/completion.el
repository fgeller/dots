;; (install 'company)
;; (setq company-minimum-prefix-length 1
;;       company-idle-delay 0.1)

;; (after 'company
;;   (setq company-backends '((:separate company-yasnippet company-capf company-files))))
;; (global-company-mode +1)

;; (after 'company
;;   (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
;;   (define-key company-active-map (kbd "<return>") nil)
;;   (define-key company-active-map (kbd "RET") nil)
;;   (setq company-auto-commit nil)
;;   (setq company-auto-complete-chars nil))

(install 'corfu)
(unless (display-graphic-p)
  (require 'popon)
  (require 'corfu-terminal)
  (corfu-terminal-mode +1))

(setq
 corfu-cycle t
 corfu-auto t
 corfu-auto-delay 0.3
 corfu-auto-prefix 1
 corfu-quit-no-match 'separator
 corfu-on-exact-match 'quit
 corfu-preview-current nil
 )

(after 'corfu
  (define-key corfu-map (kbd "<tab>") 'corfu-insert) ;; -complete
  (define-key corfu-map (kbd "<return>") nil)
  (define-key corfu-map (kbd "RET") nil))
(global-corfu-mode)

;; Don't forget that M-n binding
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
(setq xref-prompt-for-identifier nil)

(install 'vertico)
(install 'orderless)

(vertico-mode)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles . (partial-completion)))))

(setq-default vertico-group-format
			  (concat #(" %s " 0 4 (face vertico-group-title))
					  #(" ")))

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
