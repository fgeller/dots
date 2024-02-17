(set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?┃))

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

(load-theme 'ruhe t)

(unless (window-system) ;; ie -nw / terminal emacs
  (add-hook 'window-configuration-change-hook
			(lambda ()
              (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 2 2))))

(global-font-lock-mode -1)

;; https://github.com/d12frosted/homebrew-emacs-plus#system-appearance-change
(defun fg/apply-theme (appearance)
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'ruhe t))
    ('dark (load-theme 'ruhe-dark t))))

(add-hook 'ns-system-appearance-change-functions #'fg/apply-theme)

(show-paren-mode +1)
(setq show-paren-delay 0.5)
(setq show-paren-style 'parenthesis)

(install 'rainbow-mode)
(install 'leerzeichen)
(install 'nlinum)

(install 'highlight-thing)
(global-highlight-thing-mode +1)
(setq highlight-thing-prefer-active-region t)
(setq highlight-thing-delay-seconds 0.3)

(defun fg/add-todo-keyword ()
  (font-lock-add-keywords nil '(("\\(TODO\\|FIXME\\)" 1 font-lock-warning-face prepend))))
(add-hook 'font-lock-mode-hook 'fg/add-todo-keyword)

;; via https://emacs.stackexchange.com/a/62228/810
(when (>= emacs-major-version 27)
  (let ((font-list (font-family-list)))
	(dolist (fn '("Apple Color Emoji" "Noto Color Emoji"))
	  (when (member fn font-list)
		(set-fontset-font t 'symbol fn)))))

(cond (mac-p (set-face-attribute 'default nil :font "Noto Sans Mono-12:weight=book"))
	  (t (set-face-attribute 'default nil :font "Noto Sans Mono-16:weight=book")))

(setq default-frame-alist
      (append (list
			   '(left-fringe . 16)
			   '(right-fringe . 16)
			   '(vertical-scroll-bars . nil)
			   '(tool-bar-lines . 0)
			   '(internal-border-width . 0))))
