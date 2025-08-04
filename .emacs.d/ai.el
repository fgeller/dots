(use-package claude-code-ide
  :ensure (:host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  ;; (setq claude-code-ide-cli-path (expand-file-name "~/.claude/local/claude"))
  (setq claude-code-ide-cli-path (expand-file-name "~/bin/wrapper-claude"))
  (setq claude-code-ide-terminal-backend 'eat)
  (setq claude-code-ide-use-side-window nil)
  (claude-code-ide-emacs-tools-setup))

