;; cf early-init.el for more customizations

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

;; https://github.com/d12frosted/homebrew-emacs-plus#system-appearance-change
(defun fg/apply-theme (appearance)
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme 'ruhe t))
    ('dark (load-theme 'ruhe-dark t))))

(add-hook 'ns-system-appearance-change-functions #'fg/apply-theme)

(defun fg/enable-line-numbers ()
  (display-line-numbers-mode 1))

(add-hook 'prog-mode-hook 'fg/enable-line-numbers)

(show-paren-mode +1)
(setq show-paren-delay 0.1)
(setq show-paren-style 'parenthesis)

(use-package rainbow-mode 
  :ensure t
  :commands (rainbow-mode))
(use-package leerzeichen 
  :ensure t
  :commands (leerzeichen-mode))

(use-package highlight-thing
  :ensure t
  :commands (global-highlight-thing-mode)
  :config
  (global-highlight-thing-mode +1)
  (setq highlight-thing-what-thing 'symbol
	highlight-thing-prefer-active-region t
	highlight-thing-delay-seconds 0.1
	highlight-thing-all-visible-buffers-p t))

(after 'highlight-thing
  (defun highlight-thing-buffer-do (buf regex)
    (with-current-buffer buf
      (save-excursion
	(save-restriction
          (widen)
          (cond ((highlight-thing-should-narrow-to-defun-p)
		 (narrow-to-defun))
		((highlight-thing-should-narrow-to-region-p)
		 (let ((bounds (highlight-thing-narrow-bounds)))
                   (narrow-to-region (car bounds) (cdr bounds)))))
          (highlight-thing-call-highlight-regexp regex)
          (when (or highlight-thing-exclude-thing-under-point
		    (region-active-p))
	    (highlight-thing-remove-overlays-at-point regex)))))))

(defun fg/add-todo-keyword ()
  (font-lock-add-keywords nil '(("\\(TODO\\|FIXME\\)" 1 font-lock-warning-face prepend))))
(add-hook 'font-lock-mode-hook 'fg/add-todo-keyword)
