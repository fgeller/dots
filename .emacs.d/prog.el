(use-package apheleia 
  :ensure t 
  :commands (apheleia-mode))

(use-package multiple-cursors
  :ensure t
  :commands (mc/mark-next-like-this))

(use-package visual-regexp
  :ensure t
  :commands (vr/query-replace))

(use-package expand-region
  :ensure t
  :commands (er/expand-region er/contract-region)
  :config  
  ;; https://github.com/emacs-tree-sitter/elisp-tree-sitter/issues/20
  (defun fg/tree-sitter-mark-bigger-node ()
	(interactive)
	(let* ((root (tsc-root-node tree-sitter-tree))
           (node (tsc-get-descendant-for-position-range root (region-beginning) (region-end)))
           (node-start (tsc-node-start-position node))
           (node-end (tsc-node-end-position node)))
      ;; Node fits the region exactly. Try its parent node instead.
      (when (and (= (region-beginning) node-start) (= (region-end) node-end))
		(when-let ((node (tsc-get-parent node)))
          (setq node-start (tsc-node-start-position node)
				node-end (tsc-node-end-position node))))
      (set-mark node-end)
      (goto-char node-start)))

  (defun fg/add-prog-mode-expansions ()
	(make-variable-buffer-local 'er/try-expand-list)
	(setq er/try-expand-list (append
                              er/try-expand-list
                              '(fg/tree-sitter-mark-bigger-node))))

  (add-hook 'prog-mode-hook 'fg/add-prog-mode-expansions))

(use-package treesit-fold 
  :ensure t )
