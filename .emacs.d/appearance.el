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
(setq highlight-thing-prefer-active-region t)

(defun fg/add-todo-keyword ()
  (font-lock-add-keywords nil '(("\\(TODO\\|FIXME\\)" 1 font-lock-warning-face prepend))))
(add-hook 'font-lock-mode-hook 'fg/add-todo-keyword)

;; via https://emacs.stackexchange.com/a/62228/810
(when (>= emacs-major-version 27)
  (set-fontset-font t '(#x1f000 . #x1faff)
		    (font-spec :family "Noto Color Emoji")))

(setq default-frame-alist
      (append (list
	       '(font . "Roboto Mono-11:weight=book")
	       '(left-fringe . 0)
	       '(right-fringe . 0)
               '(vertical-scroll-bars . nil)
               '(tool-bar-lines . 0)
	       '(internal-border-width . 16))))
