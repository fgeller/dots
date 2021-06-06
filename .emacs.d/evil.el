(setq evil-want-keybinding nil) ;; prefer evil-collection
(install 'evil)
(evil-mode 1)

(require 'evil-workman-mode)
(evil-workman-global-mode 1)

(install 'evil-collection)
(evil-collection-init)

(install 'evil-surround)
(global-evil-surround-mode 1)

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

(install 'deadgrep)

(setq-default mode-line-format
	      '("%e"
		mode-line-front-space
		mode-line-mule-info
		mode-line-client
		mode-line-modified
		mode-line-remote
		mode-line-frame-identification
		mode-line-buffer-identification
		"   "
		mode-line-position
		evil-mode-line-tag
		(vc-mode vc-mode)
		"  "
		;;mode-line-modes
		mode-line-misc-info
		mode-line-end-spaces))

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


(setq enable-recursive-minibuffers t) 

(defconst fg/leader-keymap (make-keymap))

(define-key fg/leader-keymap (kbd "SPC") 'consult-buffer)
(define-key fg/leader-keymap (kbd "TAB") 'indent-for-tab-command)
(define-key fg/leader-keymap (kbd "cC") 'compile)
(define-key fg/leader-keymap (kbd "cc") 'recompile)  
(define-key fg/leader-keymap (kbd "g") 'consult-ripgrep)
(define-key fg/leader-keymap (kbd "G") 'deadgrep)
(define-key fg/leader-keymap (kbd "m") 'magit-status)
(define-key fg/leader-keymap (kbd "p") 'consult-yank-from-kill-ring)
(define-key fg/leader-keymap (kbd "q") 'query-replace)
(define-key fg/leader-keymap (kbd "Q") 'query-replace-regexp)
(define-key fg/leader-keymap (kbd "s") 'consult-line)
(define-key fg/leader-keymap (kbd "w") 3w-map) 
(define-key fg/leader-keymap (kbd "xb") 'switch-to-buffer)
(define-key fg/leader-keymap (kbd "xf") 'find-file)
(define-key fg/leader-keymap (kbd "xs") 'save-buffer)
(define-key fg/leader-keymap (kbd "xx") 'execute-extended-command)

(setq evil-lookup-func 'lsp-describe-thing-at-point)
(evil-define-key '(visual normal) 'global (kbd "SPC") fg/leader-keymap)

