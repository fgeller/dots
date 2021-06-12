(install 'undo-fu)
(setq evil-want-keybinding nil) ;; prefer evil-collection
(install 'evil)
(setq evil-undo-system 'undo-fu)
(evil-mode 1)

(require 'evil-workman-mode)
(evil-workman-global-mode 1)

(install 'evil-collection)
(evil-collection-init)

(install 'evil-surround)
(global-evil-surround-mode 1)

(defun fg/insert-state-mode-line ()
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

(defconst fg/toggle-keymap (make-keymap))
(let ((map fg/toggle-keymap))
  (define-key map (kbd "l") 'toggle-truncate-lines)
  (define-key map (kbd "w") 'leerzeichen-mode)
  (define-key map (kbd "d") 'toggle-debug-on-error))

(defun fg/open-line-below-normal ()
  (interactive)
  (save-excursion
    (forward-line 1)
    (end-of-line)
    (insert "\n")))

(defun fg/open-line-above-normal ()
  (interactive)
  (save-excursion
    (forward-line -1)
    (end-of-line)
    (insert "\n")))

(defun fg/duplicate-line ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (push-mark (point))
    (end-of-line)
    (kill-ring-save (point) (mark))
    (open-line 1)
    (forward-char 1)
    (yank)))

(defconst fg/elisp-eval-keymap (make-keymap))
(define-key fg/elisp-eval-keymap (kbd "r") 'eval-region)
(define-key fg/elisp-eval-keymap (kbd "b") 'eval-buffer)

(defconst fg/go-keymap (make-keymap))
(let ((map fg/go-keymap))
  (define-key map (kbd "t") 'go-run-all-tests)
  (define-key map (kbd "T") 'go-run-this-test)
  (define-key map (kbd "e") 'go-goto-first-error)
  (define-key map (kbd "n") 'go-goto-next-error)
  (define-key map (kbd "p") 'go-goto-previous-error)
  (define-key map (kbd "g") 'go-play))

(defconst fg/leader-keymap (make-keymap))
(let ((map fg/leader-keymap))
  (define-key map (kbd "SPC") 'consult-buffer)
  (define-key map (kbd "TAB") 'indent-for-tab-command)
  (define-key map (kbd "cC") 'compile)
  (define-key map (kbd "cc") 'recompile)  
  (define-key map (kbd "d") fg/go-keymap)
  (define-key map (kbd "D") 'fg/duplicate-line)
  (define-key map (kbd "e") fg/elisp-eval-keymap)
  (define-key map (kbd "g") 'consult-ripgrep)
  (define-key map (kbd "G") 'deadgrep)
  (define-key map (kbd "l") 'fg/open-line-below-normal)
  (define-key map (kbd "L") 'fg/open-line-above-normal)
  (define-key map (kbd "m") 'magit-status)
  (define-key map (kbd "p") 'consult-yank-from-kill-ring)
  (define-key map (kbd "q") 'query-replace)
  (define-key map (kbd "Q") 'query-replace-regexp)
  (define-key map (kbd "s") 'consult-line)
  (define-key map (kbd "t") fg/toggle-keymap)
  (define-key map (kbd "w") 3w-map) 
  (define-key map (kbd "xb") 'switch-to-buffer)
  (define-key map (kbd "xf") 'find-file)
  (define-key map (kbd "xs") 'save-buffer)
  (define-key map (kbd "xx") 'execute-extended-command))

(evil-define-key '(visual normal) 'global (kbd "SPC") fg/leader-keymap)

(after 'magit
  (evil-define-key 'normal magit-mode-map (kbd "SPC") fg/leader-keymap))
(after 'compile
  (evil-define-key 'normal compilation-mode-map (kbd "SPC") fg/leader-keymap))
(after 'custom
  (evil-define-key 'normal custom-mode-map (kbd "SPC") fg/leader-keymap))
(after 'help
  (evil-define-key 'normal help-mode-map (kbd "SPC") fg/leader-keymap))
(after 'view
  (evil-define-key 'normal view-mode-map (kbd "SPC") fg/leader-keymap))

(setq evil-lookup-func 'lsp-describe-thing-at-point)
