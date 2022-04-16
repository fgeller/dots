(defun ansi-colorize-buffer ()
  (require 'ansi-color)
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))
(add-hook 'compilation-filter-hook 'ansi-colorize-buffer)

(defun fg/compilation ()
  (font-lock-mode 1)
  (setq show-trailing-whitespace nil))

(add-hook 'compilation-mode-hook 'fg/compilation)

(setq compilation-scroll-output 'first-error)
(setq compilation-always-kill t)

(defun fg/project-compile ()
  (interactive)
  (let* ((default-directory (locate-dominating-file "." ".git")))
    (call-interactively 'compile)))

