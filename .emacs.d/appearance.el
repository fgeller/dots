(global-hl-line-mode +1)

(set-display-table-slot standard-display-table 'vertical-border ? )

(when tool-bar-mode (tool-bar-mode -1))
(menu-bar-mode -1)  ;; shows full-screen button for mac port

(setq-default
 use-file-dialog nil
 use-dialog-box nil
 inhibit-startup-screen t
 inhibit-startup-echo-area-message t
 truncate-lines t
 truncate-partial-width-windows nil
 transient-mark-mode t
 show-trailing-whitespace nil)

(load-custom "~/.emacs.d/mode-line.el")
(load-custom "~/.emacs.d/windows.el")

(load-theme 'ruhe t)

(show-paren-mode +1)
(setq show-paren-delay 0.5)
(setq show-paren-style 'parenthesis)

(install 'rainbow-mode)
(install 'leerzeichen)

(install 'highlight-thing)
(global-highlight-thing-mode +1)
