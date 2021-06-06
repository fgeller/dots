(setq evil-want-keybinding nil) ;; prefer evil-collection
(install 'evil)
(evil-mode 1)

(require 'evil-workman-mode)
(evil-workman-global-mode 1)

(install 'evil-collection)
(evil-collection-init)

(install 'evil-surround)
(global-evil-surround-mode 1)

(install 'deadgrep)

defun fg/insert-state-mode-line ()
  (set-face-background 'mode-line "#FFD54F")
  (set-face-foreground 'mode-line "black"))

(defun fg/normal-state-mode-line ()
  (set-face-background 'mode-line "#1565c0")
  (set-face-foreground 'mode-line "white"))

(defun fg/visual-state-mode-line ()
  (set-face-background 'mode-line "#00e676")
  (set-face-foreground 'mode-line "black"))

(add-hook 'evil-normal-state-entry-hook 'fg/normal-state-mode-line)
(add-hook 'evil-insert-state-entry-hook 'fg/insert-state-mode-line)
(add-hook 'evil-visual-state-entry-hook 'fg/visual-state-mode-line)

(defconst fg/leader-keymap (make-keymap))

(let ((map fg/leader-keymap))
  (define-key map (kbd "SPC") 'consult-buffer)
  (define-key map (kbd "TAB") 'indent-for-tab-command)
  (define-key map (kbd "cC") 'compile)
  (define-key map (kbd "cc") 'recompile)  
  (define-key map (kbd "g") 'consult-ripgrep)
  (define-key map (kbd "G") 'deadgrep)
  (define-key map (kbd "m") 'magit-status)
  (define-key map (kbd "p") 'consult-yank-from-kill-ring)
  (define-key map (kbd "q") 'query-replace)
  (define-key map (kbd "Q") 'query-replace-regexp)
  (define-key map (kbd "s") 'consult-line)
  (define-key map (kbd "w") 3w-map) 
  (define-key map (kbd "xb") 'switch-to-buffer)
  (define-key map (kbd "xf") 'find-file)
  (define-key map (kbd "xs") 'save-buffer)
  (define-key map (kbd "xx") 'execute-extended-command))

(evil-define-key '(visual normal) 'global (kbd "SPC") fg/leader-keymap)

(setq evil-lookup-func 'lsp-describe-thing-at-point)
